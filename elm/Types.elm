module Types where

data Evidence = Evidence String Bool

evidenceName (Evidence name _) = name

data Model = Ignorance
           | Evidently [Evidence]
           | Causally Evidence Evidence
           | AnyCause [Evidence] Evidence
           | AllCause [Evidence] Evidence
           | Multiple [Model]

type Potential = String
no_potential : Potential
no_potential = ""

data Population = Pop [([Evidence], Float)]

