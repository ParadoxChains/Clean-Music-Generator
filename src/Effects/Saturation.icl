implementation module Effects.Saturation
import StdEnv

e :: Int
e = 2.7182818284590

hardClippingAux :: Real Real -> Real
hardClippingAux x max
| x > max = max
| x < (-max) = (-max)
= x

hardClipping :: [Real] Real -> [Real]
hardClipping siganls max = [hardClippingAux siganl \\ siganl <- siganls]

zooz :: Real -> Real
| x > 0 = 1 - (e  ^ ~x)
= -1 + (e  ^ x)

softClippingZooz :: [Real] -> [Real]
softClippingZooz siganls = [zooz siganl \\ siganl <- siganls]

// k1 for positive part and k2 is for negative part
pirkles :: Real Real Real -> Real
pirkles x k1 k2
| x >= 0 = tanh (k1 * x) / tanh(k1)
= tanh(k1 * x) / tanh(k2)

softClippingPirkles :: [Real] Real Real -> [Real]
softClippingPirkles siganls k1 k2 = [pirkles siganl k1 k2 \\ siganl <- siganls]

jong :: Real Real -> Real
jong siganl threshold
| x < threshold = x
| x > threshold && x <= 1.0 = threshold + (x - threshold) /(1.0 + ( (x - threshold) / (1.0 - threshold)) ^ 2.0 )
| x == 1.0 = (threshold + 1.0) / 2.0

softClippingJong :: [Real] Real -> [Real]
softClippingPirkles siganls threshold = [jong siganl threshold \\ siganl <- siganls]