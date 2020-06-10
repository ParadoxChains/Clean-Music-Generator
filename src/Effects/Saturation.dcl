definition module Effects.Saturation
import StdEnv

hardClipping :: [Real] Real -> [Real]

softClippingZooz :: [Real] -> [Real]

// The second parameter is refering to the change in the positve part and
// the third parameter   is refering to the change in the negative part
// Even hormonics if second and third are different from each other 
softClippingPirkles :: [Real] Real Real -> [Real]

softClippingJong :: [Real] -> [Real]