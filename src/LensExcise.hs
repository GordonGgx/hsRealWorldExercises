{-透镜学习-}
module LensExcise where

data Position=Position {
    positionX::Double,
    positionY::Double
}deriving(Show)


xLens::Functor f=>(Double->f Double)->Position -> f Position
xLens fn p= fmap setter $ fn $ getter p
    where setter x =p{positionX=x}
          getter=positionX