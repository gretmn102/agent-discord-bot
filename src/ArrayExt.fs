[<AutoOpen>]
module ArrayExt
    /// Binary search: returns first index i where cmp(arr.[i]) >= 0.
    let binarySearchLowerBound (cmp: 'a -> int) (arr: 'a[]) : int =
        let rec loop lo hi =
            if lo > hi then lo
            else
                let mid = (lo + hi) / 2
                let c = cmp arr.[mid]
                if c < 0 then loop (mid + 1) hi
                else loop lo (mid - 1)
        loop 0 (arr.Length - 1)

    /// Binary search: returns last index i where cmp(arr.[i]) <= 0.
    let binarySearchUpperBound (cmp: 'a -> int) (arr: 'a[]) : int =
        let idx = binarySearchLowerBound (fun x -> let c = cmp x in if c > 0 then 0 else -1) arr
        if idx = 0 then 0 else idx - 1

