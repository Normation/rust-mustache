use serde::ser::{SerializeMap, SerializeSeq};
use std::collections::HashMap;
use std::fmt;
use std::{cell::RefCell, collections::BTreeMap};

// for bug!
use log::{error, log};

pub enum Data {
    Null,
    String(String, String),
    Bool(bool, String),
    Vec(Vec<Data>, String),
    Map(HashMap<String, Data>, String),
    Fun(RefCell<Box<dyn FnMut(String) -> String + Send>>),
}

impl Data {
    pub fn get_index_or_key(&self) -> &str {
        match self {
            Data::String(_, x) | Data::Bool(_, x) | Data::Vec(_, x) | Data::Map(_, x) => x,
            Data::Null | Data::Fun(_) => "",
        }
    }
}

impl serde::Serialize for Data {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match *self {
            Data::Null => serializer.serialize_none(),
            Data::String(ref v, _) => serializer.serialize_str(v),
            Data::Bool(v, _) => serializer.serialize_bool(v),
            Data::Vec(ref v, _) => {
                let mut seq = serializer.serialize_seq(Some(v.len()))?;
                for e in v {
                    seq.serialize_element(e)?;
                }
                seq.end()
            }
            Data::Map(ref v, _) => {
                let v: BTreeMap<_, _> = v.into_iter().collect();
                let mut map = serializer.serialize_map(Some(v.len()))?;
                for (k, va) in v {
                    map.serialize_entry(k, va)?;
                }
                map.end()
            }
            Data::Fun(_) => serializer.serialize_unit(),
        }
    }
}

impl PartialEq for Data {
    #[inline]
    fn eq(&self, other: &Data) -> bool {
        match (self, other) {
            (&Data::Null, &Data::Null) => true,
            (&Data::String(ref v0, _), &Data::String(ref v1, _)) => v0 == v1,
            (&Data::Bool(ref v0, _), &Data::Bool(ref v1, _)) => v0 == v1,
            (&Data::Vec(ref v0, _), &Data::Vec(ref v1, _)) => v0 == v1,
            (&Data::Map(ref v0, _), &Data::Map(ref v1, _)) => v0 == v1,
            (&Data::Fun(_), &Data::Fun(_)) => {
                bug!("Cannot compare closures");
                false
            }
            (_, _) => false,
        }
    }
}

impl fmt::Debug for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Data::Null => write!(f, "Null"),
            Data::String(ref v, ref i) => write!(f, "StrVal({})({})", v, i),
            Data::Bool(v, ref i) => write!(f, "Bool({:?})({})", v, i),
            Data::Vec(ref v, ref i) => write!(f, "VecVal({:?})({})", v, i),
            Data::Map(ref v, ref i) => write!(f, "Map({:?})({})", v, i),
            Data::Fun(_) => write!(f, "Fun(...)"),
        }
    }
}
