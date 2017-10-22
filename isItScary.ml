open Core

let create_spooky_words_regex words =
  let regexp = String.drop_suffix (List.fold_right words ~f:(fun acc curr -> acc ^ "|" ^ curr) ~init:"") 1 in
    Re2.Regex.create_exn regexp

let spooky_words = create_spooky_words_regex [
  "spook";
  "scary";
  "scarily";
  "scarier";
  "scariest";
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
  "demon";
  "boo";
  "creepy";
  "creppy";
  "spoopy";
  "satan";
  "murder";
  "death";
  "dead";
  "a+(!+)?";
  "a+h+(!+)?";
  "frankenstein";
  "mad";
  "crazy";
  "pyscho";
  "killer";
  "goblin";
  "nilbog";
  "pumpkin";
  "jack([_])?o([_])?lantern";
  "bones";
]

let not_scary_words = create_spooky_words_regex (* these words arent spooky, but code reuse *) [
  "[_A-Z]not[_A-Z]";
  "isn(')?t";
]

let its_scary str =
  let lowercase = String.lowercase str in
  (Re2.Regex.matches spooky_words lowercase) && (not (Re2.Regex.matches not_scary_words lowercase))
