import lustre/effect
import scheduler/timer

pub fn schedule(duration: Int, msg: msg) {
  use dispatch <- effect.from()
  use <- timer.set_timeout(duration)
  dispatch(msg)
}
