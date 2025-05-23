use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt::{self, Display};
use std::result::Result as StdResult;

use serde::{self, Serialize, ser};

use super::{Data, to_data};

/// Error type to represent encoding failure.
///
/// This type is not intended to be matched exhaustively as new variants
/// may be added in future without a version bump.
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    NestedOptions,
    UnsupportedType,
    MissingElements,
    KeyIsNotString,
    NoDataToEncode,
    Message(String),
}

impl serde::ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

/// Alias for a `Result` with the error type `mustache::encoder::Error`.
pub type Result<T> = StdResult<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Error::NestedOptions => "nested Option types are not supported",
                Error::UnsupportedType => "unsupported type",
                Error::MissingElements => "no elements in value",
                Error::KeyIsNotString => "key is not a string",
                Error::NoDataToEncode => "the encodable type created no data",
                Error::Message(ref s) => s,
            }
        )
    }
}

impl StdError for Error {}

#[derive(Default)]
pub struct Encoder;

impl Encoder {
    pub fn new() -> Encoder {
        Encoder
    }
}

impl serde::Serializer for Encoder {
    type Ok = Data;
    type Error = Error;

    type SerializeSeq = SerializeVec;
    type SerializeTuple = SerializeVec;
    type SerializeTupleStruct = SerializeVec;
    type SerializeTupleVariant = SerializeTupleVariant;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeMap;
    type SerializeStructVariant = SerializeStructVariant;

    fn serialize_bool(self, v: bool) -> Result<Data> {
        Ok(Data::Bool(v))
    }

    fn serialize_char(self, v: char) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_u8(self, v: u8) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_i8(self, v: i8) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_u16(self, v: u16) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_i16(self, v: i16) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_u32(self, v: u32) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_i32(self, v: i32) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_i64(self, v: i64) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_u64(self, v: u64) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_f32(self, v: f32) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_f64(self, v: f64) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_str(self, v: &str) -> Result<Data> {
        Ok(Data::String(v.to_string()))
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Data> {
        // FIXME: Perhaps this could be relaxed to just 'do nothing'
        Err(Error::UnsupportedType)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Data> {
        // FIXME: Perhaps this could be relaxed to just 'do nothing'
        Ok(Data::String(variant.to_string()))
    }

    fn serialize_unit(self) -> Result<Data> {
        Ok(Data::Null)
    }

    fn serialize_none(self) -> Result<Data> {
        Ok(Data::Null)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Data>
    where
        T: Serialize + ?Sized,
    {
        value.serialize(self)
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(SerializeStructVariant {
            name: String::from(variant),
            map: HashMap::with_capacity(len),
        })
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<Data>
    where
        T: Serialize + ?Sized,
    {
        // Ignore newtype name
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Result<Data>
    where
        T: Serialize + ?Sized,
    {
        // Ignore newtype name
        value.serialize(self)
    }

    fn serialize_bytes(self, value: &[u8]) -> Result<Data> {
        let vec = value.iter().map(|&b| Data::String(b.to_string())).collect();

        Ok(Data::Vec(vec))
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SerializeVec {
            vec: Vec::with_capacity(len.unwrap_or(0)),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(SerializeTupleVariant {
            name: String::from(variant),
            vec: Vec::with_capacity(len),
        })
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(SerializeMap {
            map: HashMap::with_capacity(len.unwrap_or(0)),
            next_key: None,
        })
    }
}

#[doc(hidden)]
pub struct SerializeVec {
    vec: Vec<Data>,
}

#[doc(hidden)]
pub struct SerializeTupleVariant {
    name: String,
    vec: Vec<Data>,
}

#[doc(hidden)]
pub struct SerializeMap {
    map: HashMap<String, Data>,
    next_key: Option<String>,
}

#[doc(hidden)]
pub struct SerializeStructVariant {
    name: String,
    map: HashMap<String, Data>,
}

impl ser::SerializeSeq for SerializeVec {
    type Ok = Data;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize + ?Sized,
    {
        self.vec.push(to_data(value)?);
        Ok(())
    }

    fn end(self) -> Result<Data> {
        Ok(Data::Vec(self.vec))
    }
}

impl ser::SerializeTuple for SerializeVec {
    type Ok = Data;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize + ?Sized,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Data> {
        ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleStruct for SerializeVec {
    type Ok = Data;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize + ?Sized,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Data> {
        ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleVariant for SerializeTupleVariant {
    type Ok = Data;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize + ?Sized,
    {
        self.vec.push(to_data(value)?);
        Ok(())
    }

    fn end(self) -> Result<Data> {
        let mut object = HashMap::new();

        object.insert(self.name, Data::Vec(self.vec));

        Ok(Data::Map(object))
    }
}

impl ser::SerializeMap for SerializeMap {
    type Ok = Data;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: Serialize + ?Sized,
    {
        match to_data(key)? {
            Data::String(s) => {
                self.next_key = Some(s);
                Ok(())
            }
            _ => Err(Error::KeyIsNotString),
        }
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: Serialize + ?Sized,
    {
        // Taking the key should only fail if this gets called before
        // serialize_key, which is a bug in the library.
        let key = self.next_key.take().ok_or(Error::MissingElements)?;
        self.map.insert(key, to_data(value)?);
        Ok(())
    }

    fn end(self) -> Result<Data> {
        Ok(Data::Map(self.map))
    }
}

impl ser::SerializeStruct for SerializeMap {
    type Ok = Data;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: Serialize + ?Sized,
    {
        ser::SerializeMap::serialize_key(self, key)?;
        ser::SerializeMap::serialize_value(self, value)
    }

    fn end(self) -> Result<Data> {
        ser::SerializeMap::end(self)
    }
}

impl ser::SerializeStructVariant for SerializeStructVariant {
    type Ok = Data;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: Serialize + ?Sized,
    {
        self.map.insert(String::from(key), to_data(value)?);
        Ok(())
    }

    fn end(self) -> Result<Data> {
        let mut object = HashMap::new();

        object.insert(self.name, Data::Map(self.map));

        Ok(Data::Map(object))
    }
}
