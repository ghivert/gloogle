import grille_pain/internals/data/toast.{type Level}

pub type Msg {
  NewToast(String, Level)
  ShowToast(Int)
  HideToast(Int, Int)
  RemoveToast(Int)
  StopToast(Int)
  ResumeToast(Int)
}
