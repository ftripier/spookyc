open Core

let create_spooky_words_regex words =
  let regexp = String.drop_suffix (List.fold_right words ~f:(fun acc curr -> acc ^ "|" ^ curr) ~init:"") 1 in
    Re2.Regex.create_exn regexp

let spooky_words = create_spooky_words_regex [
  "spooky";
  "scary";
  "scream";
  "ghost";
  "skeleton";
  "wolf";
  "jack-o-lantern";
  "bat";
  "dracula";
  "vampire";
  "witch";
  "blood";
  "dead";
  "devil";
  "666";
  "boo";
  "creepy";
  "creppy";
  "spoopy";
  "satan";
  "murder";
  "death";
  "dead";
  "A+(!+)?";
  "A+H+(!+)?";
]

let not_scary_words = create_spooky_words_regex (* these words arent spooky but code reuse lol *) [
  "not";
  "isn(')?t";
]

let its_scary str =
  (Re2.Regex.matches spooky_words str) && (not (Re2.Regex.matches not_scary_words str))
