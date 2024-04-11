import gleam/option.{type Option}

pub type Data {
  DataNil
  DataBool(String)
  DataConstant(String)
  DataBitArray(String)
  DataUtfCodepoint(String)
  DataString(String)
  DataNumber(String)
  DataTuple(List(Data))
  DataList(List(Data))
  DataCustomType(String, List(#(Option(String), Data)))
  DataDict(List(#(Data, Data)))
  DataSet(List(Data))
  DataRegex(String)
  DataDate(String)
  DataFunction(String)
  DataObject(String, List(#(Data, Data)))
}

@external(javascript, "../../tardis.ffi.mjs", "inspect")
pub fn inspect(value: a) -> Data

@external(javascript, "../../tardis.ffi.mjs", "stringify")
pub fn stringify(value: a) -> String
