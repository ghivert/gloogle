import { Result$Ok, Result$Error } from "./gleam.mjs";
import {
  Sign$Positive,
  Sign$Negative,
  Sign$isPositive,
  Sign$isNegative,
} from "./tom.mjs";

// We can't represent positive/negative NaNs in JS so we must have a representation for them
const negativeNaN = Symbol("-NaN");
const positiveNaN = Symbol("+NaN");

export function nan_to_dynamic(sign) {
  if (Sign$isPositive(sign)) {
    return positiveNaN;
  } else if (Sign$isNegative(sign)) {
    return negativeNaN;
  } else {
    // Should never happen by the type system
    throw "value is not a nan";
  }
}

export function nan_from_dynamic(value) {
  if (value == positiveNaN) {
    return Result$Ok(Sign$Positive());
  } else if (value == negativeNaN) {
    return Result$Ok(Sign$Negative());
  } else {
    // Value here is a placeholder
    return Result$Error(Sign$Positive());
  }
}

export function infinity_to_dynamic(sign) {
  if (Sign$isPositive(sign)) {
    return Infinity;
  } else if (Sign$isNegative(sign)) {
    return -Infinity;
  } else {
    // Should never happen by the type system
    throw "value is not an infinity";
  }
}

export function infinity_from_dynamic(value) {
  if (value == Infinity) {
    return Result$Ok(Sign$Positive());
  } else if (value == -Infinity) {
    return Result$Ok(Sign$Negative());
  } else {
    // Value here is a placeholder
    return Result$Error(Sign$Positive());
  }
}
