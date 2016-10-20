module Pico.FlowView where

import Pico.AbsPico
import Control.Monad.State
import Text.PrettyPrint.HughesPJ

type Visualizer = State Int FlowChart
type FlowChart = ([Box], [Arrow])
type Box = (Id, BoxType)
type Id = String 
data BoxType
 = Start
 | End
 | Decision Text
 | Activity Text
type Text = String 
type Arrow = ((Id, FromType), Id)
data FromType
 = FromStart
 | FromActivity
 | FromYes
 | FromNo


writeFlowChart :: FlowChart -> String -> IO ()
writeFlowChart fc file
 = do
      let s = showFlowChart fc
      writeFile file s

showFlowChart :: FlowChart -> String
showFlowChart (bs,as) = show main
 where
  main =
                  text "digraph"
              <+> text "FlowChart"
              <+> text "{"
              $$ nest 1 (vcat (map box bs))
              $$ nest 1 (vcat (map arrow as))
              $$ text "}"

box :: Box -> Doc

box (i, Start)
 =     text i
   <+> text "[label=\"Start\", shape=box, style=bold];"

box (i, End)
 = text i <+> text "[label=\"End\", shape=box, style=bold];"

box (i, Decision s)
 = text i <+> text "[label=\"" <+> text s <+> text "\", shape=diamond];"

box (i, Activity s)
 = text i <+> text "[label=\"" <+> text s <+> text "\", shape=box];"

arrow :: Arrow -> Doc
arrow ((from,ty), to)
 =     text from
   <+> text "->" 
   <+> text to 
   <+> text "[label=\"" <+> text label 
   <+> text "\", headport=" <+> text headport 
   <+> text ", tailport=" <+> text tailport 
   <+> text "]"
 where
  label = case ty of
           FromYes -> "Yes"
           FromNo -> "No"
           _ -> ""
  headport = "n"       
  tailport = case ty of
              FromYes -> "sw"
              FromNo -> "se"
              _ -> "s"       

-- Generate an id
nextId :: State Int String
nextId = do
            i <- get
            let i' = i+1
            put i'
            return ("id" ++ show i')

-- Addition on flowcharts
add :: FlowChart -> FlowChart -> FlowChart
add (bs1, as1) (bs2, as2) = (bs1 ++ bs2, as1 ++ as2)

toFlowChart :: Program -> String -> IO ()
toFlowChart (Program decls stmts) file = writeFlowChart main file
 where
  main = fst $ runState main' (0::Int)
  main' = do
   startId <- nextId
   endId <- nextId
   let start = (startId, Start)
   let end = (endId, End)
   (bs, as) <- stms (startId, FromStart) endId stmts
   return ([start, end]++bs, as)
  stms from to (s@(Assignment n e):stmts)
   = do
        actId <- nextId
        let act = (actId, Activity (show ("stmt")))
        (bs, as) <- stms (actId, FromActivity) to stmts
        return ([act]++bs, [(from, actId)]++as)
  stms from to (IfThen c ss1 :stmts)
   = do
        decId <- nextId
        let dec = (decId, Decision (show ("expr ifthen")))
        (bs1, as1) <- stms (decId, FromYes) to (ss1:[])
        return ([dec]++bs1, [(from, decId)]++as1)
  stms from to (IfThenElse c ss1 ss2:stmts)
   = do
        decId <- nextId
        let dec = (decId, Decision (show ("expr ifthenelse")))
        (bs1, as1) <- stms (decId, FromYes) to (ss1:[])
        (bs2, as2) <- stms (decId, FromNo) to (ss2:[])
        return ([dec]++bs1++bs2, [(from, decId)]++as1++as2)
  stms from@(fromId, fromType) to (While c body:stmts)
   = do
        decId <- nextId
        let dec = (decId, Decision (show ("expr while")))
        (bs, as) <- stms (decId, FromYes) fromId (body:[])
        return ( 
                 [dec]++bs,
                 [(from, decId)]++as++[((decId, FromNo), to)]
               )
  stms from to []
   = return ([], [(from, to)])
