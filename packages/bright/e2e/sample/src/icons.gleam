import icons/book_open
import icons/check
import icons/copy
import icons/github
import icons/home
import sketch as s
import sketch/lustre/element/html as h
import sketch/size.{px}

pub fn small(icon) {
  s.class([s.width(px(24)), s.height(px(24))])
  |> h.div([], [icon])
}

pub fn tiny(icon) {
  s.class([s.width(px(12)), s.height(px(12))])
  |> h.div([], [icon])
}

pub const book_open = book_open.icon

pub const check = check.icon

pub const copy = copy.icon

pub const github = github.icon

pub const home = home.icon
