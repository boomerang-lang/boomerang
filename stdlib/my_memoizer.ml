open Util
open My_dict

module type UnfixedDataFunction = sig
  module Arg : Data
  module Result : Data
  val f : (Arg.t -> Result.t) unfixed
end

module FixMemoizerOf(F:UnfixedDataFunction) = struct
  module ResultDict = DictOf(F.Arg)(F.Result)

  let result_storage : ResultDict.t ref = ref ResultDict.empty

  let rec evaluate
      (x:F.Arg.t)
    : F.Result.t =
    begin match ResultDict.lookup (!result_storage) x with
      | None ->
        let y = F.f evaluate x in
        result_storage := ResultDict.insert (!result_storage) x y;
        y
      | Some y -> y
    end
end
