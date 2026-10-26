# Build a piece table from compact strings "id type color square".
pcs <- function(...) {
  x <- strsplit(c(...), " ", fixed = TRUE)
  data.frame(
    id = vapply(x, `[`, "", 1L), type = vapply(x, `[`, "", 2L),
    color = vapply(x, `[`, "", 3L), square_id = vapply(x, `[`, "", 4L),
    stringsAsFactors = FALSE
  )
}

# Ids of legal actions of a given type, or all.
ids_of <- function(state, type = NULL) {
  a <- g3_legal_actions(state)
  if (!is.null(type)) a <- a[a$type == type, ]
  sort(a$action_id)
}

# Destination squares of one piece's ordinary moves.
dests_of <- function(state, piece) {
  a <- g3_legal_actions(state)
  sort(unique(a$to_square[a$type == "move" & a$piece_id == piece]))
}
