:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% Function calculating length of list
lengthList([], 0).
lengthList([_|List], Len):- lengthList(List, L1), Len is L1 + 1.

% function checking if element is memember of list
memberList(X, [X|_]).
memberList(X, [_|List]):- memberList(X, List).

% function concatenating 2 lists
concatLists([],L,L).
concatLists([H1|T1],L2,[H1|TSol]) :- concatLists(T1, L2, TSol).

% Function returning the first element from a list
getHeadList([], []).
getHeadList([X|_], X).

% Function returning the last element from a list
getLastList([X], X).
getLastList([_|List], X):- getLastList(List, X).

% Function returning the list position of a given element
getListPosition(_, [], -1).
getListPosition(X, [X|_], 0).
getListPosition(E, [_|List], X):- getListPosition(E, List, X1), X is X1 + 1, !.

% Function removing the element at a given position in a list
removePosition(_, [], []).
removePosition(0, [_|List], List).
removePosition(P, [X|List], [X|RList]):- P > 0, P1 is P - 1, removePosition(P1, List, RList).

% Function replacing the element at a given position in a single board
changePosition(_, [], _, []).
changePosition(0, [_|List], NextPlayer, [NextPlayer|List]).
changePosition(P, [X|List], NextPlayer, [X|RList]):- P > 0, !, P1 is P - 1, changePosition(P1, List, NextPlayer, RList), !.

% Function replacing the board at a given position in the list of boards
changeBoards(_, _, [], _, []).
changeBoards(0, P, [H|List], NextPlayer, [ChangedBoard|List]):- changePosition(P, H, NextPlayer, ChangedBoard), !.
changeBoards(PB, P, [X|List], NextPlayer, [X|RList]):- PB > 0, !, PB1 is PB - 1, changeBoards(PB1, P, List, NextPlayer, RList), !.

% Function returning a list of games won by the player X
getXplaysWon(State, XplaysWon):- nth0(2, State, XplaysWon, _).

% Function returning a list of games won by the player 0
get0playsWon(State, OplaysWon):- nth0(3, State, OplaysWon, _).

% function returning a list of the positions of unfinished boards
getUnfinishedGames([], _, []).
getUnfinishedGames([H|UBoard], [_|Positions], AVailablePos):- H \= '', !, getUnfinishedGames(UBoard, Positions, AVailablePos).
getUnfinishedGames([_|UBoard], [P|Positions], [P|AVailablePos]):- getUnfinishedGames(UBoard, Positions, AVailablePos).

% Function returning available moves for the next player
getAvailableBoards(State, AvailableBoards):- initialState(State), !, positions(AvailableBoards).
getAvailableBoards(State, AvailableBoard):- getUnavailableBoards(State, UnavailableBoards), lengthList(UnavailableBoards, Length),
                                            Length = 8, deleteList(UnavailableBoards, [nw, n, ne, w, c, e, sw, s, se], AvailableBoards),
                                            getHeadList(AvailableBoards, AvailableBoard).
getAvailableBoards(State, AvailableBoards):- getUnavailableBoards(State, UnavailableBoards), lengthList(UnavailableBoards, Length),
                                        Length < 8, deleteList(UnavailableBoards, [nw, n, ne, w, c, e, sw, s, se], AvailableBoards).
getAvailableBoards(State, []):- getUnavailableBoards(State, UnavailableBoards), lengthList(UnavailableBoards, Length), Length = 9.

% Function returning last Board where last player made a move
getNextBoard(UnfinishedGames, (_, P), [P]):- memberList(P, UnfinishedGames), !.
getNextBoard(UnfinishedGames, (_, _), UnfinishedGames).
getNextBoard(UnfinishedGames, P, [P]):- memberList(P, UnfinishedGames), !.
getNextBoard(UnfinishedGames, _, UnfinishedGames).


% Function returning all the won boards
getPlaysWon(State, PlaysWon):- getXplaysWon(State, XplaysWon), get0playsWon(State, OplaysWon),
                            concatLists(XplaysWon, OplaysWon, PlaysWon).

% Function returning all Draws
getDraws(State, Draws):- nth0(4, State, Draws, _).

% Function returning unavailable boards for the next player
getUnavailableBoards(State, []):- initialState(State), !.
getUnavailableBoards(State, UnavailableBoards):- concatLists(PlaysWon, Draws, UnavailableBoards),
                                                getDraws(State, Draws), getPlaysWon(State, PlaysWon).

% Function returning a list with the board results
getBoardsResult([], []).
getBoardsResult([H|Boards], [R|Result]):- getBoardResult(H, R), getBoardsResult(Boards, Result).

% function returning Boards related to given Character 
getSpecificBoards(_, [], _, []).
getSpecificBoards(Char, [H|BoardResults], [_|Positions], Result):- H \= Char, !,
                                                                getSpecificBoards(Char, BoardResults, Positions, Result).
getSpecificBoards(Char, [_|BoardResults], [P|Positions], [P|Result]):- getSpecificBoards(Char, BoardResults, Positions, Result).

% function counting character occurences on a board
getNrCharacter(_, [], 0).
getNrCharacter(Char, [Char|Board], NrC):- getNrCharacter(Char, Board, Nr), NrC is Nr + 1.
getNrCharacter(Char, [H|Board], NrC):- H \= Char, !,getNrCharacter(Char, Board, NrC).

% Function returning available positions on the given board
getAvailablePositionsBoard([], _, []).
getAvailablePositionsBoard([H|Board], [_|Positions], Result):- H \= '', getAvailablePositionsBoard(Board, Positions, Result).
getAvailablePositionsBoard([_|Board], [P|Positions], [P|Result]):- getAvailablePositionsBoard(Board, Positions, Result).

% Function returning Avaliable pairs (Board, Position) on a given board
getAvailablePairsBoard(_, [], _, []).
getAvailablePairsBoard(BoardIndex, [H|Board], [_|Positions], Result):- H \= '', !,
                                                                    getAvailablePairsBoard(BoardIndex, Board, Positions, Result), !.
getAvailablePairsBoard(BoardIndex, [_|Board], [P|Positions], [(BoardIndex, P)|Result]):-
                                                                    getAvailablePairsBoard(BoardIndex, Board, Positions, Result), !.

% Function returning all available pairs for all boards
getAvailablePairs(_, [], []).
getAvailablePairs(State, [H|Boards], NewPairs):- getBoard(State, H, Board),
                                                getAvailablePairsBoard(H, Board, [nw, n, ne, w, c, e, sw, s, se], Pos),
                                                getAvailablePairs(State, Boards, Pairs), concatLists(Pos, Pairs, NewPairs).

% Function returning the number of occurences of a given character in all boards
getNrCharactersBoards(_, [], 0).
getNrCharactersBoards(Char, [H|Boards], NrC):- getNrCharacter(Char, H, Nr1), getNrCharactersBoards(Char, Boards, Nr), NrC is Nr + Nr1.

% Function returning the best player based on the numbers of Xs and 0s
getNextPlayerBasedOnBoards(State, NextPlayer):- getBoards(State, Boards), getNrCharactersBoards('x', Boards, NrX),
                                                getNrCharactersBoards(0, Boards, Nr0), NrX > Nr0, !, NextPlayer = 0.
getNextPlayerBasedOnBoards(_, 'x').

% Function returning the last position accessed
getLastMove(State, LastMove):- nth0(5, State, LastMove, _).

% Function deleting a List from another List
deleteList(_, [], []).
deleteList(List1, [X|List2], List):- memberList(X, List1), deleteList(List1, List2, List).
deleteList(List1, [X|List2], [X|List]):- \+memberList(X, List1), deleteList(List1, List2, List).

% Function returning a list of valid moves
getValidMoves(State, Moves):- getNextAvailableBoards(State, AvailableBoards), lengthList(AvailableBoards, Length), Length > 1,
                                getAvailablePairs(State, AvailableBoards, Moves).
getValidMoves(State, Moves):- getNextAvailableBoards(State, AvailableBoards), lengthList(AvailableBoards, Length), Length = 1,
                                getHeadList(AvailableBoards, BoardIndex), getBoard(State, BoardIndex, Board),
                                getAvailablePositionsBoard(Board, [nw, n, ne, w, c, e, sw, s, se], Moves).

% Function checking if UTTT game is finished
checkFinish(State):- getUBoard(State, UBoard), \+getBoardResult(UBoard, ''), !, fail.
checkFinish(_).

% initialState/1
% initialState(-State)
% Este adev??rat pentru starea ini??ial?? a jocului.
initialState([[['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', '']], 'x', [], [], _]).

% getBoards/2
% getBoards(+State, -Boards)
% Este adev??rat dac?? ??n starea State, informa??iile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legat?? la o list?? de 9 elemente, fiecare element reprezent??nd o tabl??.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din list?? este o list?? de 9 elemente, reprezent??nd
% pozi??iile de pe tabl??, ca x, 0, sau ''.
% Pozi??iile sunt ??n ordinea din lista positions (din utils.pl).
getBoards(State, Boards) :- getHeadList(State, Boards).

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adeb??rat dac?? ??n starea State, la pozi??ia UPos din tabla de UTTT, 
% se afl?? tabla individual?? cu reprezentarea din Board.
% Reprezentarea tablei este descris?? ??n predicatul getBoards/2.
getBoard(State, UPos, Board) :- nth0(Pos, [nw, n, ne, w, c, e, sw, s, se], UPos, _),
                                getBoards(State, Boards),
                                nth0(Pos, Boards, Board, _).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% ??ntoarce reprezentarea UBoard-ului, indic??nd tablele individuale c????tigate,
% remizate, sau ??nc?? ??n desf????urare. Reprezentarea este aceea??i ca a tablelor
% individuale (vezi getBoards/2).
getUBoard(State, UBoardState) :- getBoards(State, Boards), getBoardsResult(Boards, UBoardState), !.

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adev??rat dac?? ??n starea State, ??n tabla individual?? de la pozi??ia UPos ??n UBoard,
% la pozi??ia Pos pe tabl??, se afl?? simbolul Cell (x, 0, sau '').
getPos(State, Upos, Pos, Cell):- getBoard(State, Upos, Board), getListPosition(Pos, [nw, n, ne, w, c, e, sw, s, se], P),
                                    nth0(P, Board, Cell, _).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adev??rat dac?? ??n tabla individual?? reprezentat?? ??n Board, la pozi??ia Pos, 
% se afl?? simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit ??i pentru UBoard, caz 
% ??n care Cell poate fi ??i r.
getPos(Board, Pos, Cell) :- nth0(P, [nw, n, ne, w, c, e, sw, s, se], Pos, _), nth0(P, Board, Cell, _).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adev??rat dac?? ??n starea State, juc??torul care urmeaz?? este NextPlayer
% (poate fi x sau 0)..
getNextPlayer(State, NextPlayer) :- nth0(1, State, NextPlayer, _).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adev??rat dac?? ??n starea State, pozi??iile din NextBoardsPoss sunt pozi??iile 
% din UBoard ale tablelor disponibile pentru urm??toarea mutare.
getNextAvailableBoards(State, NextBoardsPos):- initialState(State), !, positions(NextBoardsPos).
getNextAvailableBoards(State, NextBoards):- getUBoard(State, UBoard), positions(Positions),
                                            getUnfinishedGames(UBoard, Positions, UnfinishedGames),
                                            getLastMove(State, Move), getNextBoard(UnfinishedGames, Move, NextBoards), !.

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adev??rat dac?? pentru o tabl?? individual?? (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dac?? juc??torul respectiv a c????tigat jocul pe tabla dat??;
% r, dac?? s-a ajuns la remiz?? (toate pozi??iile au fost completate dar
% tabla nu a fost c????tigat??);
% '', dac?? tabla nu a fost c????tigat?? ??i nu s-au completat toate pozi??iile.
% NOT??: este deja definit predicatul player_wins/2 ??n utils.pl.
getBoardResult(Board, 'x') :- player_wins('x', Board).
getBoardResult(Board, 0) :- player_wins(0, Board).
getBoardResult(Board, '') :- \+player_wins(0, Board), \+player_wins('x', Board), memberList('', Board).
getBoardResult(Board, 'r') :- \+player_wins(0, Board), \+player_wins('x', Board), \+memberList('', Board).

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adev??rat dac?? starea State corespunde st??rii jocului ??n care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost ??n 
% pozi??ia PreviousPos ??ntr-o tabl?? individual??.
% NOT??: nu conteaz?? ??n care tabl?? individual?? s-a realizat ultima mutare.
buildState(Boards, PreviousPos, State) :- getBoards(State, Boards), getLastMove(State, PreviousPos),
                                            getNextPlayerBasedOnBoards(State, NextPlayer),
                                            getNextPlayer(State, NextPlayer),
                                            getBoardsResult(Boards, BoardsResult),
                                            getSpecificBoards('x', BoardsResult, [nw, n, ne, w, c, e, sw, s, se], XBoards),
                                            getSpecificBoards(0, BoardsResult, [nw, n, ne, w, c, e, sw, s, se], OBoards),
                                            getSpecificBoards('r', BoardsResult, [nw, n, ne, w, c, e, sw, s, se], DrawBoards),
                                            getXplaysWon(State, XBoards), get0playsWon(State, OBoards), getDraws(State, DrawBoards), !.

% validMove/2
% validMove(+State, +Move)
% Este adev??rat dac?? mutarea Move este legal?? ??n starea State.
% Move este fie o pozi??ie, ??n cazul ??n care este o singur?? tabl?? disponibil??
% pentru a urm??toarea mutare din starea State, fie o pereche de pozi??ii, altfel.
validMove(State, Move):- checkFinish(State), getValidMoves(State, Moves), \+ memberList(Move, Moves), !, fail.
validMove(State, _):- checkFinish(State).

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adev??rat dac?? ??n urma aplic??rii mut??rii Move ??n starea State
% rezulta starea NewState.
% Move este fie o pozi??ie (din lista positions), ??n cazul ??n care nu sunt mai 
% multe table disponibile pentru a urm??toarea mutare din starea State,
% fie o pereche de pozi??ii, altfel.
%
% Hint: folosi??i validMove pentru a verifica mutarea ??i buildState pentru a construi o stare.
makeMove(State, (B, P), NewState):- validMove(State, (B, P)), !, getBoards(State, Boards),
                                    getNextPlayer(State, NextPlayer),
                                    getListPosition(B, [nw, n, ne, w, c, e, sw, s, se], BoardNumeric), !,
                                    getListPosition(P, [nw, n, ne, w, c, e, sw, s, se], PositionNumeric), !,
                                    changeBoards(BoardNumeric, PositionNumeric, Boards, NextPlayer, NewBoards),
                                    buildState(NewBoards, P, NewState).
makeMove(State, P, NewState):- validMove(State, P), !, getBoards(State, Boards),
                                getNextPlayer(State, NextPlayer),
                                getNextAvailableBoards(State, Next), getHeadList(Next, B),
                                getListPosition(B, [nw, n, ne, w, c, e, sw, s, se], BoardNumeric), !,
                                getListPosition(P, [nw, n, ne, w, c, e, sw, s, se], PositionNumeric), !,
                                changeBoards(BoardNumeric, PositionNumeric, Boards, NextPlayer, NewBoards),
                                buildState(NewBoards, P, NewState).
% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leag?? NextMove la urm??toarea mutare pentru starea State.
% Strategia este foarte simpl??: va fi aleas?? cea mai din st??nga-sus mutare posibil??
% (prima din lista de pozi??ii disponibile).
dummy_first(State, NextMove) :- getValidMoves(State, Moves), getHeadList(Moves, NextMove).

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leag?? NextMove la urm??toarea mutare pentru starea State.
% Strategia este foarte simpl??: va fi aleas?? cea mai din dreapta-jos mutare posibil?? 
% (ultima din lista de pozi??ii disponibile).
dummy_last(State, NextMove) :- getValidMoves(State, Moves), getLastList(Moves, NextMove).

% ======== Etapa 2

% Function returning the other player
getOtherPlayer('x', 0).
getOtherPlayer(0, 'x').

% Function checking if board is empty
checkEmpty(['', '', '', '', '', '', '', '', '']).

% Function getting corner positions
corners([nw, ne, sw, se]).

% Function getting middle position
middle(c).

% Function checking if there is not going to be a future win within the given move
checkNotWinMove(Board, Player, Move):- positions(Positions), getListPosition(Move, Positions, Index), !,
                        changePosition(Index, Board, Player, NewBoard), !, \+ player_wins(Player, NewBoard).

% Function checking if there is not going to be a future win within 1 move out of all available moves
checkNotWinBoard(Board, Player):- positions(Positions),
                        getAvailablePositionsBoard(Board, Positions, AvailablePositions), !,
                        forall(memberList(Pos, AvailablePositions), checkNotWinMove(Board, Player, Pos)).

% Function checking if boards cannot be won by the given player
checkNotWinBoards(State, LastBoard, Boards, Player):- forall((memberList(BoardPos, Boards), makeMove(State, (LastBoard, BoardPos), NewState),
                        getBoard(NewState, BoardPos, Board)), checkNotWinBoard(Board, Player)).

% Function checking if boards weren't finished yet
checkNotFinishedBoards(State, Boards):- forall((memberList(BoardPos, Boards),
                        getBoard(State, BoardPos, Board)), getBoardResult(Board, '')).

% Function eturning all moves available to win the board
getMovesWin(Board, Player, Moves):- positions(Positions),
                        getAvailablePositionsBoard(Board, Positions, AvailablePositions), 
                        findall(Move, (memberList(Move, AvailablePositions), \+ checkNotWinMove(Board, Player, Move)), Moves).


% Function returning a list of (Priority, Position) Pairs for a given board
getPriorityPairsBoard(_, _, [], []).
getPriorityPairsBoard(Board, Player, [P|Positions], [(Priority, P)|Pairs]):-
                        movePriority(Player, Board, P, Priority), getPriorityPairsBoard(Board, Player, Positions, Pairs).

getPriorityPairsBoards(_, [], _, []).
getPriorityPairsBoards(State, [B|Boards], Player, [PairsBoard|Pairs]):- positions(Positions), getBoard(State, B, Board),
                        getAvailablePositionsBoard(Board, Positions, AvailablePositions),
                        getAvailablePairsBoard(B, Board, AvailablePositions, Pairs),
                        getPriorityPairsBoard(Board, Player, AvailablePositions, PairsBoard),
                        getPriorityPairsBoards(State, Boards, Player, Pairs).

% Function returning a list of (Priority, Move) Pairs for a given state
getPriorityPairsState(_, _, _, [], []).
getPriorityPairsState(State, Player, OtherPlayer, [M|Moves], [(Priority, M)|Pairs]):- makeMove(State, M, NewState), getPositionMove(M, P),
                        getBoard(NewState, P, Board), utttMovePriority(NewState, Player, OtherPlayer, P, Board, Priority), !,
                        getPriorityPairsState(State, Player, OtherPlayer, Moves, Pairs).

% Function returning new board after given move
getChangedBoard(Board, Player, Move, NewBoard):- positions(Positions),
                                    getListPosition(Move, Positions, MoveIndex),
                                    changePosition(MoveIndex, Board, Player, NewBoard), !.

% Function extracting the next board from a move
getBoardMove(_, (B, _), B):- !.
getBoardMove(State, _, Board):- getNextAvailableBoards(State, AvailableBoards), getHeadList(AvailableBoards, Board).

% Function extracting the next position from a move
getPositionMove((_, P), P):- !.
getPositionMove(P, P).

% Functon creating pairs based on moves in a board
createPairs(_, [], []).
createPairs(Board, [M|Moves], [(Board, M)|Pairs]):- createPairs(Board, Moves, Pairs).

% Function creating (Board, Position pairs) sorted by the individual moves inside each board
getSortedPairsBoards(_, _, [], []).
getSortedPairsBoards(State, Player, [B|Boards], NewPairs):- getBoard(State, B, Board),
                            bestIndividualMoves(Player, Board, Moves), createPairs(B, Moves, Pairs),
                            getSortedPairsBoards(State, Player, Boards, RPairs),
                            concatLists(Pairs, RPairs, NewPairs).

% Function checking if a Move goes to a finished board
checkMoveNotFinishedBoard(State, Move):- getPositionMove(Move, P), getBoard(State, P, NextBoard),
                            getBoardResult(NextBoard, Result), Result = ''.

% ===================================================================================================================================

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculeaz?? prioritatea mut??rii Move pentru juc??torul Player, ??ntr-o
% tabl?? individual?? Board. Vezi enun??.
movePriority(Player, Board, Move, 0) :- getChangedBoard(Board, Player, Move, NewBoard),
                            player_wins(Player, NewBoard), !.
movePriority(Player, Board, Move, 1):- positions(Positions),
                        getOtherPlayer(Player, OtherPlayer),
                        getListPosition(Move, Positions, MoveIndex),
                        changePosition(MoveIndex, Board, OtherPlayer, NewBoard),
                        player_wins(OtherPlayer, NewBoard), !.
movePriority(_, Board, Move, 2):- checkEmpty(Board), corners(Corners),
                        memberList(Move, Corners), !.
movePriority(Player, Board, Move, 3):- \+ memberList(Player, Board),
                        middle(Move), getPos(Board, Move, Cell),
                        getOtherPlayer(Player, OtherPlayer), Cell \= OtherPlayer, !.
movePriority(Player, Board, Move, 3):- \+ memberList(Player, Board), corners(Corners),
                        memberList(Move, Corners), middle(Middle), getPos(Board, Middle, Cell),
                        getOtherPlayer(Player, OtherPlayer), Cell = OtherPlayer, !.
movePriority(Player, Board, Move, 4):- getChangedBoard(Board, Player, Move, NewBoard),
                        \+ checkNotWinBoard(NewBoard, Player), !.
movePriority(_, _, Move, 5):- corners(Corners), member(Move, Corners), !.
movePriority(_, _, _, 6).

% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leag?? Moves la o list?? cu toate mut??rile disponibile, ??n ordinea
% priorit????ii lor.
%
% Hint: construi??i o list?? de perechi (prioritate, mutare) ??i folosi??i
% sortMoves/2 pentru a ob??ine lista de mut??ri, ??n ordinea priorit????ii.
bestIndividualMoves(Player, Board, Moves):- positions(Positions),
            getAvailablePositionsBoard(Board, Positions, AvailablePositions), !,
            getPriorityPairsBoard(Board, Player, AvailablePositions, PriorityPairs),
            sortMoves(PriorityPairs, Moves), !.

% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care ??ntotdeauna ia cea mai bun?? mutare individual??.
% Dac?? sunt mai multe table disponibile, ia tabla care este cea mai bun??
% mutare individual?? ??n raport cu U-board.
narrowGreedy(State, Move):- getNextAvailableBoards(State, AvailableBoards), lengthList(AvailableBoards, Length),
                            Length = 1, !, getHeadList(AvailableBoards, BoardPos), getBoard(State, BoardPos, Board),
                            getNextPlayer(State, Player), bestIndividualMoves(Player, Board, Moves), getHeadList(Moves, Move).
narrowGreedy(State, (BoardPos, Position)) :- getUBoard(State, UBoard), getNextPlayer(State, Player), bestIndividualMoves(Player, UBoard, UMoves),
                            getHeadList(UMoves, BoardPos), getBoard(State, BoardPos, Board), bestIndividualMoves(Player, Board, Moves),
                            getHeadList(Moves, Position).

% Function calculating Move priority for the entire UTTT table
utttMovePriority(NewState, _, _, _, _, 0):- \+ checkFinish(NewState), !.
utttMovePriority(_, _, _, _, Board, 12):- getBoardResult(Board, Result), Result \= '', !.
utttMovePriority(_, Player, _, _, Board, 9):- \+ checkNotWinBoard(Board, Player), !.
utttMovePriority(_, _, OtherPlayer, _, Board, 1):-
                                getNrCharacter(OtherPlayer, Board, NrAppearances), NrAppearances = 0, !.
utttMovePriority(_, _, OtherPlayer, _, Board, 2):- 
                                getNrCharacter(OtherPlayer, Board, NrAppearances), NrAppearances = 1, !.
utttMovePriority(_, Player, OtherPlayer, _, Board, Priority):- 
                                checkNotWinBoard(Board, Player), checkNotWinBoard(Board, OtherPlayer),
                                getNrCharacter(OtherPlayer, Board, NrAppearances), NrAppearances > 2,
                                getNrCharacter(Player, Board, NrAppearancesCurrent), Priority is 3 + NrAppearancesCurrent, !.
utttMovePriority(NewState, _, OtherPlayer, _, Board, 10):- getMovesWin(Board, OtherPlayer, WinMoves),
                                \+ checkNotFinishedBoards(NewState, WinMoves), !.
utttMovePriority(NewState, Player, OtherPlayer, P, Board, 10):- getMovesWin(Board, OtherPlayer, WinMoves),
                                \+ checkNotWinBoards(NewState, P, WinMoves, Player), !.
utttMovePriority(NewState, Player, OtherPlayer, P, Board, 11):- getMovesWin(Board, OtherPlayer, WinMoves),
                                checkNotWinBoards(NewState, P, WinMoves, Player), !.
utttMovePriority(NewState, _, OtherPlayer, P, Board, 13):- \+ checkNotWinBoard(Board, OtherPlayer),
                                getUBoard(NewState, UBoard), positions(Positions), getListPosition(P, Positions, Pos),
                                changePosition(Pos, UBoard, OtherPlayer, NewUBoard), 
                                \+ getBoardResult(NewUBoard, ''), !.
utttMovePriority(_, _, _, _, _, 8).

% bestMoves/2
% bestMoves(+State, -Moves)
% Leag?? Moves la o list?? care con??ine toate mut??rile disponibile, ??n
% ordinea priorit????ii lor, dup?? ordonarea prezentat?? ??n enun??.
bestMoves(State, SortedMoves) :- getNextAvailableBoards(State, AvailableBoards), lengthList(AvailableBoards, Length), Length = 1, !,
                                getNextPlayer(State, Player), getHeadList(AvailableBoards, B), getBoard(State, B, Board),
                                bestIndividualMoves(Player, Board, Moves), getOtherPlayer(Player, OtherPlayer),
                                getPriorityPairsState(State, Player, OtherPlayer, Moves, Pairs), !,
                                sortMoves(Pairs, SortedMoves).
bestMoves(State, SortedMoves) :- getUBoard(State, UBoard), getNextPlayer(State, Player), bestIndividualMoves(Player, UBoard, AvailableBoards),
                                getOtherPlayer(Player, OtherPlayer), getSortedPairsBoards(State, Player, AvailableBoards, Moves),
                                getPriorityPairsState(State, Player, OtherPlayer, Moves, Pairs), !, sortMoves(Pairs, SortedMoves).

% greedy/2
% greedy(+State, -Move)
% Strategie care alege cea mai bun?? mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy(State, Move) :- bestMoves(State, Moves), !, getHeadList(Moves, Move).
