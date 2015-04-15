module ArrayLabels = struct
  include ArrayLabels
            
  let make ~length ~elt =
    ArrayLabels.make length elt

end

module BufferLabels = struct
  include Buffer
end
