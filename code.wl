(* ::Package:: *)

<<DatabaseLink` 

conn=OpenSQLConnection[JDBC["MySQL(Connector/J)","localhost:3306/gamehistory"],"Username"->"root","Password"->"tictactoe"];
SQLExecute[conn,"SELECT * FROM games"];

ClearAll[ShowStats];
ShowStats[]:=(Print["\:041f\:043e\:0441\:043b\:0435\:0434\:043d\:0438\:0435 5 \:0438\:0433\:0440:"];
Print[DateObject[#[[2,1]]], ", ",#[[3]] /. {1 -> "\:041a\:0440\:0435\:0441\:0442\:0438\:043a\:0438 ", -1 -> "\:041d\:043e\:043b\:0438\:043a\:0438 "},"\:0432\:044b\:0438\:0433\:0440\:0430\:043b\:0438, ", "\:0421\:043b\:043e\:0436\:043d\:043e\:0441\:0442\:044c: ", #[[4]], ", \:041f\:0430\:0440\:0442\:0438\:044f \:0434\:043b\:0438\:043b\:0430\:0441\:044c: ", TimeObject[#[[5,1]]]]&/@SQLExecute[conn,"select * from games order by gameid desc limit 5"] );


Dynamic@nolclr;
Dynamic@krestclr;
nolclr=Orange;
krestclr=Blue;
nolik=
Circle[#,0.35]&;
krestik=
{Line[{#+{0.35,0.35},#+{-0.35,-0.35}}],Line[{#+{-0.35,0.35},#+{0.35,-0.35}}]}&;


l = 5;


mask1 = ConstantArray[1,l];
mask2 = DiagonalMatrix[ConstantArray[1,l]];
mask3 = Reverse[DiagonalMatrix[ConstantArray[1,l]]];


ClearAll[UpdateTimer];
UpdateTimer[] := (refTimes = AbsoluteTime[];refTimem = 0;refTimeh = 0;
timer = Row[{Dynamic[Refresh[If[refTimeh>24,refTimeh=0];IntegerString[refTimeh,10,2],UpdateInterval->3600]],":",
Dynamic[Refresh[If[refTimem>59,refTimem=0;refTimeh++];IntegerString[refTimem,10,2],UpdateInterval->1]],":",
Dynamic[Refresh[If[Floor[AbsoluteTime[]-refTimes]>59,refTimes=AbsoluteTime[]; refTimem++];IntegerString[Floor[AbsoluteTime[]-refTimes],10,2],UpdateInterval->1]]}]);

ClearAll[TakeBack];
TakeBack[] := (hintkr = {}; hintnol = {}; If[!isOver,If[difficulty == 0 \[And] Length[cellas] > 0, (UpdateField[#,0]; If[MemberQ[kr,#], kr = Most[kr], nol = Most[nol]]; currentMove*=-1)& /@ Keys[Take[cellas, -1]]; cellas = Drop[cellas,-1],
 If[Length[cellas]>1, (UpdateField[#,0]; If[MemberQ[kr,#], kr = Most[kr], nol = Most[nol]]; currentMove*=-1)& /@ Keys[Take[cellas,-2]]; cellas = Drop[cellas,-2]]]];If[cellas == <||>, timer = "00:00:00"]);

ClearAll[UpdateScale];
UpdateScale[speed_,delta_] := (scale = Clip[(1+0.015*speed)*Boole[delta>0]*scale +  (1-0.015*speed)*Boole[delta<0]*scale + scale*Boole[delta == 0.], {1,3.5}])

ClearAll[UpdateSpeed];
UpdateSpeed[scalelist_] :=(speed =3-2Exp[-10 ((scalelist[[-1]]-scalelist[[1]]))^2])

ClearAll[UpdateDelta]
UpdateDelta[scalelist_]:=delta = (scalelist[[-1]]-scalelist[[-2]])

ClearAll[UpdateScalelist];
UpdateScalelist[mousepos_] := AppendTo[scalelist, -1+2*mousepos[[2]]]

ClearAll[UpdateLims];
UpdateLims[center_, scale_, width_]:= ({{minlimx, maxlimx}, {minlimy, maxlimy}} = Round[center]+  Ceiling[3.5*width])

ClearAll[UpdateDraglist];
UpdateDraglist[mousepos_] := ( AppendTo[draglist, mousepos])

ClearAll[CalculateCenter];
CalculateCenter[draglist_, scale_] := If[!MemberQ[draglist, None],({dx,dy} = draglist[[-1]]-draglist[[1]];center-={{dx,dx}, {dy,dy}}/scale)]

ClearAll[DrawLines];
DrawLines[minlimx_, maxlimx_, minlimy_, maxlimy_] := {InfiniteLine[{#+0.5,0.5}, {0,1}]& /@ Range[minlimx,maxlimx],InfiniteLine[{0.5,#+0.5}, {1,0}]& /@ Range[minlimy,maxlimy]}

ClearAll[HighlightRectangle];
HighlightRectangle[mousepos_]:={Opacity[If[mousepos==None,0,1]],Rectangle[Round[mousepos/. None -> (scale*range[[All,2]] + 2)]-{0.5,0.5}]}

ClearAll[DetermineVictory];
DetermineVictory[] := (res = {ListConvolve[{ConstantArray[1,l]},gamefield],ListConvolve[Transpose[{ConstantArray[1,l]}],gamefield],ListConvolve[DiagonalMatrix[ConstantArray[1,l]],gamefield],ListConvolve[Reverse[DiagonalMatrix[ConstantArray[1,l]]], gamefield]};
If[MemberQ[res, l,3], victoryPos=First@Position[res,l];KrestikiWin[],
 If[MemberQ[res,-l,3], victoryPos=First@Position[res,-l];NolikiWin[]]])

ClearAll[KrestikiWin];
KrestikiWin[] := ( krest++;
 CalculateVictoryPoints[];isOver = True;timer = Row[{IntegerString[refTimeh,10,2],":", IntegerString[refTimem,10,2],":",IntegerString[seconds = Floor[AbsoluteTime[]-refTimes],10,2] }];
 SQLExecute[conn,"INSERT INTO games (date, result, mode, gametime) VALUES ( NOW(), 1, "<>ToString[difficulty]<>", '"<>ToString[Row[{IntegerString[refTimeh,10,2],":", IntegerString[refTimem,10,2],":",IntegerString[seconds,10,2] }]]<>"' )"];);

ClearAll[NolikiWin];
NolikiWin[] := (noliki++; CalculateVictoryPoints[];isOver = True;timer = Row[{IntegerString[refTimeh,10,2],":", IntegerString[refTimem,10,2],":",IntegerString[seconds = Floor[AbsoluteTime[]-refTimes],10,2] }];
SQLExecute[conn,"INSERT INTO games (date, result, mode, gametime) VALUES ( NOW(), -1, "<>ToString[difficulty]<>", '"<>ToString[Row[{IntegerString[refTimeh,10,2],":", IntegerString[refTimem,10,2],":",IntegerString[seconds,10,2] }]]<>"' )"];);

ClearAll[DrawVictoryLine];
DrawVictoryLine[] := {Opacity[If[isOver,.7,0]],(Line[If[!isOver,{{(scale*range[[All,2]] + 2),(scale*range[[All,2]] + 2)},{(scale*range[[All,2]] + 2),(scale*range[[All,2]] + 2)}}, {victorypoint1, victorypoint2}]])}

ClearAll[CalculateVictoryPoints];
CalculateVictoryPoints[]:=(
victorypoint1=FromMatrixToGraphics[Rest@victoryPos];
Piecewise[{
{victorypoint2 =victorypoint1+{l-1,0},victoryPos[[1]]==1},
{victorypoint2 =victorypoint1+{0,-l+1},victoryPos[[1]]==2},
{victorypoint2 =victorypoint1+{l-1,-l+1},victoryPos[[1]]==3},
{victorypoint1=victorypoint1+{0,-l+1};victorypoint2 =victorypoint1+{l-1,l-1},victoryPos[[1]]==4}}];)

ClearAll[FirstMoveKrestik];
FirstMoveKrestik[]:=(a="\:0412\:044b \:0438\:0433\:0440\:0430\:0435\:0442\:0435 \:0437\:0430 \:043a\:0440\:0435\:0441\:0442\:0438\:043a";);

ClearAll[FirstMoveNolik];
FirstMoveNolik[]:=(If[difficulty == 0, currentMove = -1];DetermineVictory[difficulty];a="\:0412\:044b \:0438\:0433\:0440\:0430\:0435\:0442\:0435 \:0437\:0430 \:043d\:043e\:043b\:0438\:043a";If[difficulty != 0, UpdateTimer[]]);

ClearAll[CheckPos];
CheckPos[ pos_] := And@@Thread[Abs[#]<=( Length[gamefield]-1)/2&[pos]]

ClearAll[UpdateFieldInside]
UpdateFieldInside[ pos_, currentMove_] := (ReplacePart[gamefield,Reverse[(Abs[pos-{-1,1}((Length[gamefield]-1)/2)]+1)]-> currentMove ])

ClearAll[UpdateField];
UpdateField[ pos_, currentMove_] :=If[CheckPos[pos],gamefield= UpdateFieldInside[pos, currentMove]; cellas[pos] = currentMove;, gamefield = ArrayPad[gamefield,1]; UpdateField[pos, currentMove]]

ClearAll[CheckPosMatrix]
CheckPosMatrix[ pos_] := And@@Thread[(0 <#<=Length[gamefield])&[pos]]

ClearAll[UpdateFieldInsideMatrix]
UpdateFieldInsideMatrix[ pos_, currentMove_] := (gamefield = ReplacePart[gamefield,pos-> currentMove ])

ClearAll[UpdateFieldMatrix];
UpdateFieldMatrix[ pos_, currentMove_] :=If[CheckPosMatrix[pos],UpdateFieldInsideMatrix[pos, currentMove]; cellas[FromMatrixToGraphics[pos]] = currentMove, gamefield = ArrayPad[gamefield,1]; UpdateFieldMatrix[pos+1, currentMove]]

ClearAll[ClearField];
ClearField[] := (gamefield = If[Mod[l,2] == 1, ConstantArray[0,{Max[l,11],Max[l,11]}],ConstantArray[0,{Max[l+1,11],Max[l+1,11]}]]; kr = {}; nol = {};hintkr={};hintnol={}; cellas = <||>; currentMove = 1;isOver=False;
 timer = Row[{"00",":","00",":","00"}];
a =Column[{"\:0412\:044b \:0438\:0433\:0440\:0430\:0435\:0442\:0435 \:0437\:0430 ",
Row[{Button["\:043a\:0440\:0435\:0441\:0442\:0438\:043a",FirstMoveKrestik[]],Button["\:043d\:043e\:043b\:0438\:043a",FirstMoveNolik[]]},"      "]},Center,Spacings->1];)

ClearAll[Init];
Init[] := (gamefield = If[Mod[l,2] == 1, ConstantArray[0,{Max[l,11],Max[l,11]}],ConstantArray[0,{Max[l+1,11],Max[l+1,11]}]];kr = {};nol = {};cellas = <||>;
isOver = False;
difficulty=0;
currentMove = 1;
hintkr={};
hintnol={};
scale = 3;speed = 1;scalelist = {} ; krest = 0; noliki = 0;
delta = 0.;
scalelist = {};draglist = {};
dx = 0; dy = 0;
center = {{0,0},{0,0}};width = {{-2,2}, {-2,2}};
range = {{-2,2},{-2,2}};
{{minlimx ,maxlimx },{minlimy ,maxlimy }}=4range;)


ClearAll[Game];
Game[] := (Init[];Row[{EventHandler[Dynamic@Graphics[{LightGray,HighlightRectangle[MousePosition["Graphics"]],Darker[Gray], Thick,DrawLines[minlimx,maxlimx,minlimy, maxlimy] ,AbsoluteThickness[2],Green,krestik/@hintkr,nolik/@hintnol,Lighter[Blue,.4], krestclr,krestik/@kr,Lighter[Orange,.4], nolclr, nolik/@nol,Red,AbsoluteThickness[4],Opacity[.5],DrawVictoryLine[]},PlotRange-> (center+scale*width), Background-> White,ImageSize->400], { "MouseClicked":>(mousepos = Round[MousePosition["Graphics"]];MakeOneMoveAndBot[mousepos,difficulty]),"MouseDragged" :> (UpdateDraglist[MousePosition["Graphics"]];CalculateCenter[draglist, scale] ),
"MouseUp" :> (draglist={};
UpdateLims[center,scale,width]), 
{"MouseUp",2} :> (scalelist={};UpdateLims[center, scale, width]),{"MouseDragged",2} :> (UpdateScalelist[MousePosition["CellScaled"]]; If[Length[scalelist] > 1,UpdateDelta[scalelist];
UpdateSpeed[scalelist]];
UpdateScale[speed,delta]
 )}],
Column[{ timer = Row[{"00",":","00",":","00"}]; Dynamic[timer], 
a = Column[{"\:0412\:044b \:0438\:0433\:0440\:0430\:0435\:0442\:0435 \:0437\:0430 ",
Row[{Button["\:043a\:0440\:0435\:0441\:0442\:0438\:043a",FirstMoveKrestik[]],Button["\:043d\:043e\:043b\:0438\:043a",FirstMoveNolik[]]},"      "]},Center,Spacings->1];Dynamic[a],
Column[{"\:0423\:0440\:043e\:0432\:0435\:043d\:044c \:0441\:043b\:043e\:0436\:043d\:043e\:0441\:0442\:0438",
Row[{Slider[Dynamic[difficulty],{0,3,1}],Dynamic[difficulty]},ImageMargins -> 10]},Center,Spacings->1],
Row[{Button["\:0412\:0435\:0440\:043d\:0443\:0442\:044c \:0445\:043e\:0434",TakeBack[],ImageSize->{90,40}],Button["\:0420\:0435\:0441\:0442\:0430\:0440\:0442",ClearField[],ImageSize->{90,40}]}, "      "],
Button["\:041f\:043e\:0434\:0441\:043a\:0430\:0437\:043a\:0430",Hint[],ImageSize->{200,40}],
Column[{
Column[{"    \:0414\:043b\:0438\:043d\:0430 \:043f\:043e\:0431\:0435\:0434\:043d\:043e\:0439 \:043a\:043e\:043c\:0431\:0438\:043d\:0430\:0446\:0438\:0438",Row[{Slider[Dynamic[l],{3,10,1}, ImageSize->Small],Dynamic[l]}]}, Alignment->Center, Spacings->1],Column[{Row[{ColorSlider[Dynamic@nolclr, AppearanceElements -> "Swatch"],Dynamic@Graphics[{nolclr,Circle[{0,0},0.17]},Axes->False, ImageSize->30]}, "     "],
Row[{ColorSlider[Dynamic@krestclr,AppearanceElements -> "Swatch"],Dynamic@Graphics[{krestclr,Line[{{0.17,0.17},{-0.17,-0.17}}],Line[{{-0.17,0.17},{0.17,-0.17}}]},Axes->False, ImageSize->30]}, "     "]}]}, Alignment->Center],
Button["\:041f\:043e\:043a\:0430\:0437\:0430\:0442\:044c \:0441\:0442\:0430\:0442\:0438\:0441\:0442\:0438\:043a\:0443",ShowStats[],ImageSize->{200,40}]
},

Center,Spacings->0.5]}])

(*\:0421\:043b\:043e\:0436\:043d\:044b\:0439 \:0431\:043e\:0442*)

ClearAll[MakeOneMoveAndBot];
MakeOneMoveAndBot[mousepos_, difficulty_:0] := If[IsAvailableQ[mousepos],If[currentMove == 1,AppendTo[kr,mousepos],AppendTo[nol,mousepos]];If[(cellas == <||>), UpdateTimer[]]; UpdateField[mousepos, currentMove];hintkr={};hintnol={};
If[MatchQ[a  ,_Column],FirstMoveKrestik[]];
 currentMove *=-1;DetermineVictory[difficulty]]

DetermineVictory[0] := DetermineVictory[];

DetermineVictory[difficulty_] := (res = {ListConvolve[{mask1},gamefield],ListConvolve[Transpose[{mask1}],gamefield],ListConvolve[DiagonalMatrix[ConstantArray[1,l]],gamefield],ListConvolve[Reverse[DiagonalMatrix[ConstantArray[1,l]]], gamefield]}; If[MemberQ[res, l(-currentMove),3], victoryPos=First@Position[res,l(-currentMove)];If[currentMove == 1, NolikiWin[],KrestikiWin[]], Bot[difficulty]; DetermineVictory[]])

ClearAll[Bot]
Bot[difficulty_] := 
(move = BotMove[difficulty];UpdateField[move, currentMove]; If[currentMove == 1, AppendTo[kr, move], AppendTo[nol, move]];currentMove*=-1;)

ClearAll[BotMove]
BotMove[difficulty_] := (difficulty /.{1 :> EasyBotMove[], 2 :> MediumBotMove[], 3 :> HardBotMove[]})

FindNeighbors[{i_,j_}]:=Complement[Flatten[Table[{i+di,j+dj},{di,-1,1},{dj,-1,1}],1],{{i,j}}];

ClearAll[FromGraphicsToMatrix];
FromGraphicsToMatrix[point_] :=(Reverse[(Abs[point-{-1,1}((Length[gamefield]-1)/2)]+1)])

ClearAll[FromMatrixToGraphics];
FromMatrixToGraphics[point_] := ({1,-1}Reverse[point-((Length[gamefield]+1)/2)])

ClearAll[IsAvailableQ];
IsAvailableQ[{x_,y_}] := If[isOver,False,MissingQ[cellas[{x,y}]]];

ClearAll[FindAvailableCells];
FindAvailableCells[]:= ( Union@@(Select[FindNeighbors[#], IsAvailableQ]& /@ Keys[cellas]))

ClearAll[GetMovePoints];
GetMovePoints[{x_,y_}] := (Max[GetMovePoints[{x,y},currentMove],(GetMovePoints[{x,y},-currentMove]-10) ])

GetMovePoints[{x_,y_}, player_] := PointsInAllDirectionsForPlayer[{x,y},player];

ConstructFreeLine[point_, direction_]:= ((point + # direction)&/@Range[l-1])

ClearAll[ConstructLineWithSymbols];
ConstructLineWithSymbols[point_, direction_, player_]:=(TakeWhile[ GetValueOfPoint/@ ConstructFreeLine[point,direction], # != -player&])

ClearAll[DirectionPoints];
DirectionPoints[point_, direction_, player_,1] := (
If[segment1 == {},0,LengthWhile[Most[segment1], #==player&]])

DirectionPoints[point_, direction_, player_,2] := (
If[segment2 == {},0,LengthWhile[Most[segment2], #==player&]])

ClearAll[GetValueOfPoint];
GetValueOfPoint[point_] := (cellas[point] /. _Missing -> 0)

ClearAll[ConsecutiveLinePoints];
ConsecutiveLinePoints[point_, direction_, player_] :=((2+DirectionPoints[point, -direction, player,1]+ DirectionPoints[point, direction, player,2]))

ClearAll[SlidingLinePoints];
SlidingLinePoints[point_, direction_, player_]:= (Max[Count[#,player]&/@Partition[line,  l, 1]])

ClearAll[TotalLinePoints];
TotalLinePoints[point_,direction_, player_] := (
line = Join[segment1 =ConstructLineWithSymbols[point, -direction, player], {player},segment2 = ConstructLineWithSymbols[point, direction, player] ];If[Length[line] >=l,Max[90*ConsecutiveLinePoints[point,direction, player], 100*SlidingLinePoints[point,direction, player]],0])

ClearAll[PointsInAllDirectionsForPlayer];
PointsInAllDirectionsForPlayer[point_,player_] := (Max[TakeLargest[(scores = (TotalLinePoints[point, #, player])& /@ {{1,1}, {1,0}, {1,-1}, {0,1}}),2]+{0,1}] + (scores . scores)/100.^2)

ClearAll[HardBotMove];
HardBotMove[] := If[cellas == <||>, {0,0},(cells =( Union@@(Select[FindNeighbors[#], IsAvailableQ]& /@ Keys[cellas])); Last[cells[[Ordering[GetMovePoints /@ cells]]]])]

ClearAll[Hint];
Hint[]:=If[!isOver,(hint=HardBotMove[];If[currentMove==1,hintkr={hint},hintnol={hint}])];

(*\:0421\:0440\:0435\:0434\:043d\:0438\:0439 \:0431\:043e\:0442*)

ClearAll[UpdateFieldMedium];
UpdateFieldMedium[ pos_, currentMove_] :=If[CheckPos[pos],gamefield= UpdateFieldInside[pos, currentMove], gamefield = ArrayPad[gamefield,1]; UpdateFieldMedium[pos, currentMove]]

ClearAll[MediumBotMove];
MediumBotMove[] :=
If[cellas == <||>, {0,0},(cells =FindAvailableCells[];UpdateFieldMedium[#,0]& /@ cells; matcells = FromGraphicsToMatrix/@cells;extendedGamefield = ArrayPad[gamefield,l];f1 = (#==-currentMove)&;
f2 = (#==currentMove)&;
res1 = {LengthWhile[extendedGamefield[[#[[1]], #[[2]]-1;;#[[2]]-l+1;;-1]], f1]+LengthWhile[extendedGamefield[[#[[1]], #[[2]]+1;;#[[2]]+l-1]], f1],LengthWhile[extendedGamefield[[#[[1]]-1;;#[[1]]-l+1;;-1, #[[2]]]], f1]+LengthWhile[extendedGamefield[[#[[1]]+1;;#[[1]]+l-1, #[[2]]]], f1],LengthWhile[Reverse[Diagonal[extendedGamefield[[#[[1]]-l+1;;#[[1]]-1, #[[2]]-l+1;;#[[2]]-1]]]], f1]+LengthWhile[Diagonal[extendedGamefield[[#[[1]]+1;;#[[1]]+l-1, #[[2]]+1;;#[[2]]+l-1]]], f1],LengthWhile[Reverse[Diagonal[Reverse[extendedGamefield[[#[[1]]+1;;#[[1]]+l-1, #[[2]]-l+1;;#[[2]]-1]]]]], f1]+LengthWhile[Diagonal[Reverse[extendedGamefield[[#[[1]]-l+1;;#[[1]]-1, #[[2]]+1;;#[[2]]+l-1]]]], f1]}& /@ (matcells+l);
res2 = {LengthWhile[extendedGamefield[[#[[1]], #[[2]]-1;;#[[2]]-l+1;;-1]], f2]+LengthWhile[extendedGamefield[[#[[1]], #[[2]]+1;;#[[2]]+l-1]], f2],LengthWhile[extendedGamefield[[#[[1]]-1;;#[[1]]-l+1;;-1, #[[2]]]], f2]+LengthWhile[extendedGamefield[[#[[1]]+1;;#[[1]]+l-1, #[[2]]]], f2],LengthWhile[Reverse[Diagonal[extendedGamefield[[#[[1]]-l+1;;#[[1]]-1, #[[2]]-l+1;;#[[2]]-1]]]], f2]+LengthWhile[Diagonal[extendedGamefield[[#[[1]]+1;;#[[1]]+l-1, #[[2]]+1;;#[[2]]+l-1]]], f2],LengthWhile[Reverse[Diagonal[Reverse[extendedGamefield[[#[[1]]+1;;#[[1]]+l-1, #[[2]]-l+1;;#[[2]]-1]]]]], f2]+LengthWhile[Diagonal[Reverse[extendedGamefield[[#[[1]]-l+1;;#[[1]]-1, #[[2]]+1;;#[[2]]+l-1]]]], f2]}& /@ (matcells+l);
defenceflag =Max[ Max/@res1];
attackflag =Max[ Max/@res2];
If[attackflag >= defenceflag, move = (cells)[[Last[PositionLargest[Max/@res2]]]], move = (cells)[[Last[PositionLargest[Max/@res1]]]]]
)]


(*\:041b\:0435\:0433\:043a\:0438\:0439 \:0431\:043e\:0442*)

FindNeighbors[i_,j_]:=Complement[Flatten[Table[{i+di,j+dj},{di,-1,1},{dj,-1,1}],1],{{i,j}}];

ClearAll[EasyBotMove];
EasyBotMove[] :=If[cellas == <||>,{0,0},If[Keys[Select[cellas, #==currentMove&]]=={}, RandomChoice[FindNeighbors[RandomChoice[Keys[Select[cellas, #==-currentMove&]]]]],(RandomChoice[Select[Flatten[FindNeighbors/@Keys[Select[cellas, #==currentMove&]],1], IsAvailableQ]])]]

