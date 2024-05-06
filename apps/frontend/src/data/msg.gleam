import lustre_http as http

pub type Msg {
  None
  SubmitSearch
  SearchResults(Result(String, http.HttpError))
  UpdateInput(String)
}
