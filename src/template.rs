use std::collections::BTreeMap;
use std::collections::HashMap;
use std::io::Write;
use std::mem;
use std::str;
use std::vec;

use compiler::Compiler;
// for bug!
use log::{error, log};
use parser::Token;
use serde::Serialize;

use super::{to_data, Context, Data, Error, Result};

/// `Template` represents a compiled mustache file.
#[derive(Debug, Clone)]
pub struct Template {
    ctx: Context,
    tokens: Vec<Token>,
    partials: HashMap<String, Vec<Token>>,
}

/// Construct a `Template`. This is not part of the impl of Template so it is
/// not exported outside of mustache.
pub fn new(ctx: Context, tokens: Vec<Token>, partials: HashMap<String, Vec<Token>>) -> Template {
    Template {
        ctx: ctx,
        tokens: tokens,
        partials: partials,
    }
}

impl Template {
    /// Renders the template with the `Encodable` data.
    pub fn render<W, T>(&self, wr: &mut W, data: &T) -> Result<()>
    where
        W: Write,
        T: Serialize,
    {
        let data = to_data(data)?;
        self.render_data(wr, &data)
    }

    /// Renders the template with the `Data`.
    pub fn render_data<W: Write>(&self, wr: &mut W, data: &Data) -> Result<()> {
        let mut render_ctx = RenderContext::new(self);
        let mut stack = vec![data];

        render_ctx.render(wr, &mut stack, &self.tokens)
    }

    /// Renders the template to a `String` with the `Encodable` data.
    pub fn render_to_string<T: Serialize>(&self, data: &T) -> Result<String> {
        let mut output = Vec::new();
        self.render(&mut output, data)?;
        String::from_utf8(output).map_err(|_| Error::InvalidStr)
    }

    /// Renders the template to a `String` with the `Data`.
    pub fn render_data_to_string(&self, data: &Data) -> Result<String> {
        let mut output = Vec::new();
        self.render_data(&mut output, data)?;
        String::from_utf8(output).map_err(|_| Error::InvalidStr)
    }
}

struct RenderContext<'a> {
    template: &'a Template,
    indent: String,
    line_start: bool,
}

impl<'a> RenderContext<'a> {
    fn new(template: &'a Template) -> RenderContext<'a> {
        RenderContext {
            template: template,
            indent: "".to_string(),
            line_start: true,
        }
    }

    fn render<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        tokens: &[Token],
    ) -> Result<()> {
        for token in tokens.iter() {
            self.render_token(wr, stack, token)?;
        }

        Ok(())
    }

    fn render_token<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        token: &Token,
    ) -> Result<()> {
        match *token {
            Token::Text(ref value) => self.render_text(wr, value),
            Token::JSON(ref path, _) => self.render_json(wr, stack, path, false),
            Token::JSONMulti(ref path, _) => self.render_json(wr, stack, path, true),
            Token::TopJSON(ref path, _) => self.render_json(wr, stack, path, false),
            Token::TopJSONMulti(ref path, _) => self.render_json(wr, stack, path, true),
            Token::TopSection(ref path, _, ref children, ref otag, _, ref src, _, ref ctag) => {
                self.render_section_top(wr, stack, path, children, src, otag, ctag)
            }
            Token::EscapedTag(ref path, _) => self.render_etag(wr, stack, path),
            Token::UnescapedTag(ref path, _) => self.render_utag(wr, stack, path),
            Token::Section(ref path, true, ref children, _, _, _, _, _) => {
                self.render_inverted_section(wr, stack, path, children)
            }
            Token::Section(ref path, false, ref children, ref otag, _, ref src, _, ref ctag) => {
                self.render_section(wr, stack, path, children, src, otag, ctag)
            }
            Token::Partial(ref name, ref indent, _) => self.render_partial(wr, stack, name, indent),
            Token::IncompleteSection(..) => {
                bug!("render_token should not encounter IncompleteSections");
                Err(Error::IncompleteSection)
            }
        }
    }

    fn write_tracking_newlines<W: Write>(&mut self, wr: &mut W, value: &str) -> Result<()> {
        wr.write_all(value.as_bytes())?;
        self.line_start = match value.chars().last() {
            None => self.line_start, // None == ""
            Some('\n') => true,
            _ => false,
        };

        Ok(())
    }

    fn write_indent<W: Write>(&mut self, wr: &mut W) -> Result<()> {
        if self.line_start {
            wr.write_all(self.indent.as_bytes())?;
        }

        Ok(())
    }

    fn render_text<W: Write>(&mut self, wr: &mut W, value: &str) -> Result<()> {
        // Indent the lines.
        if self.indent.is_empty() {
            return self.write_tracking_newlines(wr, value);
        } else {
            let mut pos = 0;
            let len = value.len();

            while pos < len {
                let v = &value[pos..];
                let line = match v.find('\n') {
                    None => {
                        let line = v;
                        pos = len;
                        line
                    }
                    Some(i) => {
                        let line = &v[..i + 1];
                        pos += i + 1;
                        line
                    }
                };

                if line.as_bytes()[0] != b'\n' {
                    self.write_indent(wr)?;
                }

                self.write_tracking_newlines(wr, line)?;
            }
        }

        Ok(())
    }

    fn render_etag<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        path: &[String],
    ) -> Result<()> {
        let mut bytes = vec![];

        self.render_utag(&mut bytes, stack, path)?;

        for b in bytes {
            match b {
                b'<' => wr.write_all(b"&lt;")?,
                b'>' => wr.write_all(b"&gt;")?,
                b'&' => wr.write_all(b"&amp;")?,
                b'"' => wr.write_all(b"&quot;")?,
                b'\'' => wr.write_all(b"&#39;")?,
                _ => wr.write_all(&[b])?,
            }
        }

        Ok(())
    }

    fn render_utag<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        path: &[String],
    ) -> Result<()> {
        match self.find(path, stack) {
            None => {}
            Some(value) => {
                self.write_indent(wr)?;

                // Currently this doesn't allow Option<Option<Foo>>, which
                // would be un-nameable in the view anyway, so I'm unsure if it's
                // a real problem. Having {{foo}} render only when `foo = Some(Some(val))`
                // seems unintuitive and may be surprising in practice.
                if let Data::Null = *value {
                    return Ok(());
                }

                match *value {
                    Data::String(ref value) => {
                        self.write_tracking_newlines(wr, value)?;
                    }

                    // etags and utags use the default delimiter.
                    Data::Fun(ref fcell) => {
                        let f = &mut *fcell.borrow_mut();
                        let tokens = self.render_fun("", "{{", "}}", f)?;
                        self.render(wr, stack, &tokens)?;
                    }

                    Data::Bool(ref b) => {
                        self.write_tracking_newlines(wr, &b.to_string())?;
                    }

                    ref value => {
                        bug!("render_utag: unexpected value {:?}", value);
                    }
                }
            }
        };

        Ok(())
    }

    fn write_tracking_newlines_json<T: serde::Serialize, W: Write>(
        &mut self,
        wr: &mut W,
        data: T,
        pretty: bool,
    ) -> Result<()> {
        let json = match pretty {
            true => serde_json::to_string_pretty(&data).unwrap(),
            false => serde_json::to_string(&data).unwrap().to_string(),
        };
        self.write_tracking_newlines(wr, &json)?;
        Ok(())
    }

    fn render_json<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        path: &[String],
        pretty: bool,
    ) -> Result<()> {
        if !path.is_empty() && path[0] == "-top-" && !stack.is_empty() {
            let v = stack.pop().unwrap();
            self.write_tracking_newlines_json(wr, &v, pretty)?;
        } else {
            match self.find(path, stack) {
                None => {}
                Some(value) => {
                    self.write_indent(wr)?;
                    // Currently this doesn't allow Option<Option<Foo>>, which
                    // would be un-nameable in the view anyway, so I'm unsure if it's
                    // a real problem. Having {{foo}} render only when `foo = Some(Some(val))`
                    // seems unintuitive and may be surprising in practice.
                    if let Data::Null = *value {
                        return Ok(());
                    }

                    match *value {
                        Data::String(ref v) => {
                            self.write_tracking_newlines(wr, v)?;
                        }
                        Data::Bool(ref v) => {
                            self.write_tracking_newlines(wr, &v.to_string())?;
                        }
                        Data::Fun(ref fcell) => {
                            let f = &mut *fcell.borrow_mut();
                            let tokens = self.render_fun("", "{{", "}}", f)?;
                            self.render(wr, stack, &tokens)?;
                        }
                        Data::Vec(ref v) => {
                            self.write_tracking_newlines_json(wr, v, pretty)?;
                        }
                        Data::Map(ref v) => {
                            let v: BTreeMap<_, _> = v.into_iter().collect();
                            self.write_tracking_newlines_json(wr, &v, pretty)?;
                        }
                        ref value => {
                            bug!("render_json: unexpected value {:?}", value);
                        }
                    }
                }
            };
        }
        Ok(())
    }

    fn render_inverted_section<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        path: &[String],
        children: &[Token],
    ) -> Result<()> {
        match self.find(path, stack) {
            None => {}
            Some(&Data::Null) => {}
            Some(&Data::Bool(false)) => {}
            Some(&Data::Vec(ref xs)) if xs.is_empty() => {}
            Some(_) => {
                return Ok(());
            }
        }

        self.render(wr, stack, children)
    }

    fn render_section_top<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        _path: &[String],
        children: &[Token],
        src: &str,
        otag: &str,
        ctag: &str,
    ) -> Result<()> {
        let i = stack.clone();
        let nstack = i.iter();
        for v in nstack {
            match Some(v) {
                None => {}
                Some(value) => {
                    match *value {
                        Data::Null => {
                            // do nothing
                        }
                        Data::Bool(_) => {
                            stack.push(value);
                            self.render(wr, stack, children)?;
                            stack.pop();
                        }
                        // Data::Bool(false) => (),
                        Data::String(ref val) => {
                            if !val.is_empty() {
                                stack.push(value);
                                self.render(wr, stack, children)?;
                                stack.pop();
                            }
                        }
                        Data::Vec(ref vs) => {
                            for v in vs.iter() {
                                stack.push(v);
                                self.render(wr, stack, children)?;
                                stack.pop();
                            }
                        }
                        Data::Map(_) => {
                            stack.push(value);
                            self.render(wr, stack, children)?;
                            stack.pop();
                        }
                        Data::Fun(ref fcell) => {
                            let f = &mut *fcell.borrow_mut();
                            let tokens = self.render_fun(src, otag, ctag, f)?;
                            self.render(wr, stack, &tokens)?;
                        }
                    }
                }
            };
        }
        Ok(())
    }

    fn render_section<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        path: &[String],
        children: &[Token],
        src: &str,
        otag: &str,
        ctag: &str,
    ) -> Result<()> {
        match self.find(path, stack) {
            None => {}
            Some(value) => {
                match *value {
                    Data::Null => {
                        // do nothing
                    }
                    Data::Bool(true) => self.render(wr, stack, children)?,
                    Data::Bool(false) => (),
                    Data::String(ref val) => {
                        if !val.is_empty() {
                            stack.push(value);
                            self.render(wr, stack, children)?;
                            stack.pop();
                        }
                    }
                    Data::Vec(ref vs) => {
                        for v in vs.iter() {
                            stack.push(v);
                            self.render(wr, stack, children)?;
                            stack.pop();
                        }
                    }
                    Data::Map(_) => {
                        stack.push(value);
                        self.render(wr, stack, children)?;
                        stack.pop();
                    }
                    Data::Fun(ref fcell) => {
                        let f = &mut *fcell.borrow_mut();
                        let tokens = self.render_fun(src, otag, ctag, f)?;
                        self.render(wr, stack, &tokens)?;
                    }
                }
            }
        };
        Ok(())
    }

    fn render_partial<W: Write>(
        &mut self,
        wr: &mut W,
        stack: &mut Vec<&Data>,
        name: &str,
        indent: &str,
    ) -> Result<()> {
        match self.template.partials.get(name) {
            None => (),
            Some(ref tokens) => {
                let mut indent = self.indent.clone() + indent;

                mem::swap(&mut self.indent, &mut indent);
                self.render(wr, stack, &tokens)?;
                mem::swap(&mut self.indent, &mut indent);
            }
        };

        Ok(())
    }

    fn render_fun(
        &self,
        src: &str,
        otag: &str,
        ctag: &str,
        f: &mut Box<dyn FnMut(String) -> String + Send + 'static>,
    ) -> Result<Vec<Token>> {
        let src = f(src.to_string());

        let compiler = Compiler::new_with(
            self.template.ctx.clone(),
            src.chars(),
            self.template.partials.clone(),
            otag.to_string(),
            ctag.to_string(),
        );

        let (tokens, _) = compiler.compile()?;
        Ok(tokens)
    }

    fn find<'c>(&self, path: &[String], stack: &mut Vec<&'c Data>) -> Option<&'c Data> {
        // If we have an empty path, we just want the top value in our stack.
        if path.is_empty() {
            match stack.last() {
                None => {
                    return None;
                }
                Some(data) => {
                    return Some(*data);
                }
            }
        }

        // Otherwise, find the stack that has the first part of our path.
        let mut value = None;

        for data in stack.iter().rev() {
            match **data {
                Data::Map(ref m) => {
                    if let Some(v) = m.get(&path[0]) {
                        value = Some(v);
                        break;
                    }
                }
                _ => { /* continue searching the stack */ }
            }
        }

        // Walk the rest of the path to find our final value.
        let mut value = match value {
            Some(value) => value,
            None => {
                return None;
            }
        };

        for part in path[1..].iter() {
            match *value {
                Data::Map(ref m) => match m.get(part) {
                    Some(v) => {
                        value = v;
                    }
                    None => {
                        return None;
                    }
                },
                _ => {
                    return None;
                }
            }
        }

        Some(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::compile_str;

    use super::*;

    fn render_data(template: &Template, data: &Data) -> String {
        let mut bytes = vec![];
        template
            .render_data(&mut bytes, data)
            .expect("Failed to render data");
        String::from_utf8(bytes).expect("Failed ot encode as String")
    }

    #[test]
    fn test_json_simple_string() {
        let template = compile_str("Hello, {{$name}}").expect("failed to compile");
        let mut ctx = HashMap::new();
        ctx.insert("name".to_string(), Data::String("Ferris".to_string()));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "Hello, Ferris".to_string()
        );
    }

    #[test]
    fn test_json_simple_vec() {
        let template = compile_str("{{$v}}").expect("failed to compile");
        let v = vec![
            Data::String("A".to_string()),
            Data::String("B".to_string()),
            Data::String("C".to_string()),
        ];
        let mut ctx = HashMap::new();
        ctx.insert("v".to_string(), Data::Vec(v));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "[\"A\",\"B\",\"C\"]".to_string()
        );
    }

    #[test]
    fn test_json_simple_map() {
        let template = compile_str("{{$v}}").expect("failed to compile");
        let mut v = HashMap::new();
        v.insert("k1".to_string(), Data::String("A".to_string()));
        v.insert("k2".to_string(), Data::String("B".to_string()));
        let mut ctx = HashMap::new();
        ctx.insert("v".to_string(), Data::Map(v));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "{\"k1\":\"A\",\"k2\":\"B\"}".to_string()
        );
    }

    #[test]
    fn test_json_bool() {
        let template = compile_str("{{$b}}").expect("failed to compile");
        let b = true;
        let mut ctx = HashMap::new();
        ctx.insert("b".to_string(), Data::Bool(b));
        assert_eq!(render_data(&template, &Data::Map(ctx)), "true".to_string());
    }

    #[test]
    fn test_bool() {
        let template = compile_str("{{b}}").expect("failed to compile");
        let b = true;
        let mut ctx = HashMap::new();
        ctx.insert("b".to_string(), Data::Bool(b));
        assert_eq!(render_data(&template, &Data::Map(ctx)), "true".to_string());
    }

    #[test]
    fn test_top_json() {
        let template = compile_str("{{$-top-}}").expect("failed to compile");
        let b = true;
        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Data::String("String".to_string()));
        ctx.insert("b".to_string(), Data::Bool(b));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "{\"a\":\"String\",\"b\":true}".to_string()
        );
    }

    #[test]
    fn test_dot_json() {
        let template = compile_str("{{$.}}").expect("failed to compile");
        let b = true;
        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Data::String("String".to_string()));
        ctx.insert("b".to_string(), Data::Bool(b));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "{\"a\":\"String\",\"b\":true}".to_string()
        );
    }

    #[test]
    fn test_top_json_multi() {
        let template = compile_str("{{%-top-}}").expect("failed to compile");
        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Data::String("String".to_string()));
        ctx.insert("b".to_string(), Data::Bool(true));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "{\n  \"a\": \"String\",\n  \"b\": true\n}".to_string()
        );
    }

    #[test]
    fn test_dot_json_multi() {
        let template = compile_str("{{%.}}").expect("failed to compile");
        let b = true;
        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Data::String("String".to_string()));
        ctx.insert("b".to_string(), Data::Bool(b));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "{\n  \"a\": \"String\",\n  \"b\": true\n}".to_string()
        );
    }

    #[test]
    fn test_section() {
        let template = compile_str("{{#a}}{{$.}} {{/a}}").expect("failed to compile");
        let mut ctx = HashMap::new();
        let v = vec![
            Data::String("String1".to_string()),
            Data::String("String2".to_string()),
            Data::String("String3".to_string()),
        ];
        ctx.insert("a".to_string(), Data::Vec(v));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "String1 String2 String3 "
        );
    }

    #[test]
    fn test_top_section() {
        let template = compile_str("{{#-top-}}{{$.}}{{/-top-}}").expect("failed to compile");
        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Data::String("String".to_string()));
        ctx.insert("b".to_string(), Data::Bool(true));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "{\"a\":\"String\",\"b\":true}".to_string()
        );
    }

    #[test]
    fn test_top_section_multi() {
        let template = compile_str("{{#-top-}}{{%.}}{{/-top-}}").expect("failed to compile");
        let mut ctx = HashMap::new();
        ctx.insert("a".to_string(), Data::String("String".to_string()));
        ctx.insert("b".to_string(), Data::Bool(true));
        assert_eq!(
            render_data(&template, &Data::Map(ctx)),
            "{\n  \"a\": \"String\",\n  \"b\": true\n}".to_string()
        );
    }

    // #[test]
    // fn test_vec_at() {
    //     let template = compile_str("{{#v}}{{@}} {{/v}}").expect("failed to compile");
    //     let v = vec![
    //         Data::String("A".to_string()),
    //         Data::String("B".to_string()),
    //         Data::String("C".to_string()),
    //     ];
    //     let mut ctx = HashMap::new();
    //     ctx.insert("v".to_string(), Data::Vec(v));
    //     assert_eq!(
    //         render_data(&template, &Data::Map(ctx)),
    //         "0 1 2 ".to_string()
    //     );
    // }
    //
    // #[test]
    // fn test_top_section_at() {
    //     let template = compile_str("{{#-top-}}{{$@}} {{/-top-}}").expect("failed to compile");
    //     let mut ctx = HashMap::new();
    //     ctx.insert("a".to_string(), Data::String("String".to_string()));
    //     ctx.insert("b".to_string(), Data::Bool(true));
    //     assert_eq!(render_data(&template, &Data::Map(ctx)), "a b ".to_string());
    // }
}
