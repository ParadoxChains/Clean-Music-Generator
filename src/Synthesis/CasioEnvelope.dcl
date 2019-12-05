definition module Synthesis.CasioEnvelope

:: CasioCZ = {delayCZ :: Real // Seconds
                ,attack1CZ :: Real // Seconds
                ,holdCZ :: Real // Seconds
                ,decay1CZ :: Real // Seconds
                ,breakpoint1CZ :: Real // %
                ,decay2CZ :: Real // Seconds
                ,breakpoint2CZ :: Real // %
                ,decay3CZ :: Real // Seconds
                ,breakpoint3CZ :: Real // %
                ,attack2CZ :: Real // Seconds
                ,sustainCZ :: Real // %
                ,releaseDecayCZ :: Real // Seconds
                ,releaseBreakpoint1CZ :: Real // %
                ,releaseAttackCZ :: Real // Seconds
                ,releaseBreakpoint2CZ :: Real // %
                ,releaseCZ :: Real // Seconds
                }

getCasioCZ :: Real CasioCZ -> [Real]