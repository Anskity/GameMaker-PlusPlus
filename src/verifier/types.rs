pub enum DataType {
    Number,
    String,
    Array(Box<DataType>),
    RawStruct,
    Struct(Vec<(String, Box<DataType>)>),
}
