An algorithm that finds initial starting states garunteeing that the first user 
wins the game.

The game invovles two players. 
Given a simply directed graph G, two players take turns to make their moves.
In each player's turn, he/she makes a move by starting from most recently visited vertex(noted as v') of the opponent, choosing an unused edge going out from v' and using the edge to visit another vertex (noted as u).
In the future, no one may use the edge again.

If the opponent is left with no unused edge to use, the other player wins.

Implement a function that finds all the vertex that garuntees the first player is to win. Use data structure Map and idea of lazy evaluation to increase the efficience of the function.
