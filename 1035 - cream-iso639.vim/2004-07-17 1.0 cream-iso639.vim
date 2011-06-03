"=====================================================================
" cream-iso639.vim -- Exchange ISO639 language names and abbreviations
"
" Cream -- An easy-to-use configuration of the famous Vim text  editor
" [ http://cream.sourceforge.net ] Copyright (C) 2002-2004  Steve Hall
"
" License:
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of  the  License,  or
" (at your option) any later version.
" [ http://www.gnu.org/licenses/gpl.html ]
"
" This program is distributed in the hope that it will be useful,  but
" WITHOUT  ANY  WARRANTY;  without  even  the  implied   warranty   of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the  GNU
" General Public License for more details.
"
" You should have received a copy of the GNU  General  Public  License
" along with  this  program;  if  not,  write  to  the  Free  Software
" Foundation,  Inc.,  59  Temple  Place  -  Suite  330,   Boston,   MA
" 02111-1307, USA.
"
"
" Updated:  2004-07-17 17:10:04-0400
"
" Description:
" Find ISO639 language abbreviations or name when passed the other.
"
" Source:
"   http://www.loc.gov/standards/iso639-2/langcodes.html
"
"
" Usage:
"
"   Cream_iso639(word, ...)
"
" o Test {word} for match of ISO639-compliant language name, 3- or
"   2-letter abbreviation:
"   * Returns name if {word} matches either abbreviation.
"   * Returns 3-letter if {word} matches name.
"   * Returns 0 if no match is made.
" o Use {optional} argument to force a given return:
"   * Returns 3-letter if {optional} is "3" and {word} matches name, 2-
"     or 3-letter.
"   * Returns 2-letter if {optional} is "2" and {word} matches name, 2-
"     or 3-letter. But if no 2-letter exists, the 3-letter is returned
"     if there is a match.
"   * Returns name if {optional} is "name" and {word} matches name, 2-
"     or 3-letter.
"   * Returns 0 if no match is made.
" o Matching is case-insensitive. But return values are capitalized
"   according to the standard. (Name is title case, abbreviations are
"   lower case.)
" o In a number of cases, ISO639-2 allows multiple descriptive names
"   for a language. 
"   * When passed the matching abbreviation, the function will return
"     the preferred description (listed first in the ISO list). 
"   * Conversely, any of the listed descriptions can be matched, not
"     just the preferred.
"
" Examples:
"
" Condition where 2- and 3-letter abbreviations exist a language:
"
"   :echo Cream_iso639("abk")                returns "Abkhazian"
"   :echo Cream_iso639("ab")                 returns "Abkhazian"
"   :echo Cream_iso639("Abkhazian")          returns "abk"
"
"   :echo Cream_iso639("Abkhazian", "3")     returns "abk"
"   :echo Cream_iso639("abk", "3")           returns "abk"
"   :echo Cream_iso639("ab", "3")            returns "abk"
"   :echo Cream_iso639("Abkhazian", "2")     returns "ab"
"   :echo Cream_iso639("abk", "2")           returns "ab"
"   :echo Cream_iso639("ab", "2")            returns "ab"
"   :echo Cream_iso639("Abkhazian", "name")  returns "Abkhazian"
"   :echo Cream_iso639("abk", "name")        returns "Abkhazian"
"   :echo Cream_iso639("ab", "name")         returns "Abkhazian"
"
" Condition where a 2-letter abbreviation doesn't exist for language:
"
"   :echo Cream_iso639("Achinese")           returns "ace"
"   :echo Cream_iso639("ace")                returns "Achinese"
"   :echo Cream_iso639("Achinese", "2")      returns "ace"
"   :echo Cream_iso639("ace", "2")           returns "ace"
"
" Name or abbreviation unmatched:
"
"   :echo Cream_iso639("not-a-name")         returns 0
"   :echo Cream_iso639("not")                returns 0
"

" Cream_iso639() {{{1
function! Cream_iso639(word, ...)
" test strings

	if a:0 > 0
		if  a:1 == "2"
		\|| a:1 == "3"
		\|| a:1 == "name"
			let optn = a:1
		else
			let optn = ""
		endif
	else
		let optn = ""
	endif

	let a = s:Cream_iso639_test(a:word, "Abkhazian", "abk", "ab", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Achinese", "ace", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Acoli", "ach", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Adangme", "ada", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Adyghe", "ady", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Adygei", "ady", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Afar", "aar", "aa", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Afrihili", "afh", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Afrikaans", "afr", "af", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Afro-Asiatic", "afa", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Akan", "aka", "ak", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Akkadian", "akk", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Albanian", "alb", "sq", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Albanian", "sqi", "sq", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Aleut", "ale", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Algonquian", "alg", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Altaic", "tut", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Amharic", "amh", "am", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Apache", "apa", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Arabic", "ara", "ar", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Aragonese", "arg", "an", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Aramaic", "arc", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Arapaho", "arp", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Araucanian", "arn", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Arawak", "arw", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Armenian", "arm", "hy", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Armenian", "hye", "hy", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Artificial", "art", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Assamese", "asm", "as", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Asturian", "ast", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bable", "ast", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Athapascan", "ath", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Australian", "aus", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Austronesian", "map", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Avaric", "ava", "av", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Avestan", "ave", "ae", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Awadhi", "awa", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Aymara", "aym", "ay", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Azerbaijani", "aze", "az", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Balinese", "ban", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Baltic", "bat", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Baluchi", "bal", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bambara", "bam", "bm", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bamileke", "bai", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Banda", "bad", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bantu", "bnt", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Basa", "bas", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bashkir", "bak", "ba", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Basque", "baq", "eu", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Basque", "eus", "eu", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Batak", "btk", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Beja", "bej", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Belarusian", "bel", "be", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bemba", "bem", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bengali", "ben", "bn", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Berber", "ber", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bhojpuri", "bho", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bihari", "bih", "bh", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bikol", "bik", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bini", "bin", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bislama", "bis", "bi", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Blin", "byn", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bilin", "byn", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bosnian", "bos", "bs", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Braj", "bra", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Breton", "bre", "br", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Buginese", "bug", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bulgarian", "bul", "bg", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Buriat", "bua", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Burmese", "bur", "my", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Burmese", "mya", "my", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Caddo", "cad", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Carib", "car", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Catalan", "cat", "ca", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Valencian", "cat", "ca", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Caucasian", "cau", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Cebuano", "ceb", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Celtic", "cel", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Central American Indian", "cai", "", optn)   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chagatai", "chg", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chamic", "cmc", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chamorro", "cha", "ch", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chechen", "che", "ce", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Cherokee", "chr", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Cheyenne", "chy", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chibcha", "chb", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chichewa", "nya", "ny", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chewa", "nya", "ny", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nyanja", "nya", "ny", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chinese", "chi", "zh", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chinese", "zho", "zh", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chinook", "chn", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chipewyan", "chp", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Choctaw", "cho", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Church Slavic", "chu", "cu", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Old Slavonic", "chu", "cu", optn)            | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Church Slavonic", "chu", "cu", optn)         | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Old Bulgarian", "chu", "cu", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Old Church Slavonic", "chu", "cu", optn)     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chuukese", "chk", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chuvash", "chv", "cv", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Classical Newari", "nwc", "", optn)          | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Old Newari", "nwc", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Classical Nepal Bhasa", "nwc", "", optn)     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Coptic", "cop", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Cornish", "cor", "kw", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Corsican", "cos", "co", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Cree", "cre", "cr", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Creek", "mus", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Creoles and pidgins", "crp", "", optn)       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Creoles and pidgins, English-based", "cpe", "", optn)     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Creoles and pidgins, French-based", "cpf", "", optn)      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Creoles and pidgins, Portuguese-based", "cpp", "", optn)  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Crimean Tatar", "crh", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Crimean Turkish", "crh", "", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Croatian", "scr", "hr", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Croatian", "hrv", "hr", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Cushitic", "cus", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Czech", "cze", "cs", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Czech", "ces", "cs", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dakota", "dak", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Danish", "dan", "da", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dargwa", "dar", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dayak", "day", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Delaware", "del", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dinka", "din", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Divehi", "div", "dv", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dogri", "doi", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dogrib", "dgr", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dravidian", "dra", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Duala", "dua", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dutch, Middle", "dum", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dutch", "dut", "nl", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dutch", "nld", "nl", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Flemish", "dut", "nl", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dyula", "dyu", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Dzongkha", "dzo", "dz", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Efik", "efi", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Egyptian", "egy", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ekajuk", "eka", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Elamite", "elx", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "English", "eng", "en", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "English, Middle", "enm", "", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "English, Old", "ang", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Erzya", "myv", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Esperanto", "epo", "eo", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Estonian", "est", "et", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ewe", "ewe", "ee", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ewondo", "ewo", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Fang", "fan", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Fanti", "fat", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Faroese", "fao", "fo", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Fijian", "fij", "fj", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Finnish", "fin", "fi", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Finno-Ugrian", "fiu", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Fon", "fon", "", optn)                       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "French", "fre", "fr", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "French", "fra", "fr", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "French, Middle", "frm", "", optn)            | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "French, Old", "fro", "", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Frisian", "fry", "fy", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Friulian", "fur", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Fulah", "ful", "ff", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ga", "gaa", "", optn)                        | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gaelic", "gla", "gd", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Scottish Gaelic", "gla", "gd", optn)         | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gallegan", "glg", "gl", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ganda", "lug", "lg", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gayo", "gay", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gbaya", "gba", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Geez", "gez", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Georgian", "geo", "ka", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Georgian", "kat", "ka", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "German", "ger", "de", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "German", "deu", "de", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "German, Middle High", "gmh", "", optn)       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "German, Old High", "goh", "", optn)          | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Germanic", "gem", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gilbertese", "gil", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gondi", "gon", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gorontalo", "gor", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gothic", "got", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Grebo", "grb", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Greek, Ancient", "grc", "", optn)            | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Greek, Modern", "gre", "el", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Greek, Modern", "ell", "el", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Guarani", "grn", "gn", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gujarati", "guj", "gu", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gwich´in", "gwi", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Haida", "hai", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Haitian", "hat", "ht", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Haitian Creole", "hat", "ht", optn)          | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hausa", "hau", "ha", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hawaiian", "haw", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hebrew", "heb", "he", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Herero", "her", "hz", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hiligaynon", "hil", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Himachali", "him", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hindi", "hin", "hi", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hiri Motu", "hmo", "ho", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hittite", "hit", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hmong", "hmn", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hungarian", "hun", "hu", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Hupa", "hup", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Iban", "iba", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Icelandic", "ice", "is", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Icelandic", "isl", "is", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ido", "ido", "io", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Igbo", "ibo", "ig", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ijo", "ijo", "", optn)                       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Iloko", "ilo", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Inari Sami", "smn", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Indic", "inc", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Indo-European", "ine", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Indonesian", "ind", "id", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ingush", "inh", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Interlingua", "ina", "ia", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Interlingue", "ile", "ie", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Inuktitut", "iku", "iu", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Inupiaq", "ipk", "ik", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Iranian", "ira", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Irish", "gle", "ga", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Irish, Middle", "mga", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Irish, Old", "sga", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Iroquoian", "iro", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Italian", "ita", "it", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Japanese", "jpn", "ja", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Javanese", "jav", "jv", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Judeo-Arabic", "jrb", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Judeo-Persian", "jpr", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kabardian", "kbd", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kabyle", "kab", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kachin", "kac", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kalaallisut", "kal", "kl", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Greenlandic", "kal", "kl", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kalmyk", "xal", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kamba", "kam", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kannada", "kan", "kn", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kanuri", "kau", "kr", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kara-Kalpak", "kaa", "", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Karachay-Balkar", "krc", "", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Karen", "kar", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kashmiri", "kas", "ks", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kashubian", "csb", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kawi", "kaw", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kazakh", "kaz", "kk", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Khasi", "kha", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Khmer", "khm", "km", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Khoisan", "khi", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Khotanese", "kho", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kikuyu", "kik", "ki", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Gikuyu", "kik", "ki", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kimbundu", "kmb", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kinyarwanda", "kin", "rw", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kirghiz", "kir", "ky", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Klingon", "tlh", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "tlhlngan-Hol", "tlh", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Komi", "kom", "kv", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kongo", "kon", "kg", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Konkani", "kok", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Korean", "kor", "ko", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kosraean", "kos", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kpelle", "kpe", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kru", "kro", "", optn)                       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kuanyama", "kua", "kj", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kwanyama", "kua", "kj", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kumyk", "kum", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kurdish", "kur", "ku", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kurukh", "kru", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Kutenai", "kut", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ladino", "lad", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lahnda", "lah", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lamba", "lam", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lao", "lao", "lo", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Latin", "lat", "la", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Latvian", "lav", "lv", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lezghian", "lez", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Limburgan", "lim", "li", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Limburger", "lim", "li", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Limburgish", "lim", "li", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lingala", "lin", "ln", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lithuanian", "lit", "lt", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lojban", "jbo", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Low German", "nds", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Low Saxon", "nds", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "German, Low", "nds", "", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Saxon, Low", "nds", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lower Sorbian", "dsb", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lozi", "loz", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Luba-Katanga", "lub", "lu", optn)            | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Luba-Lulua", "lua", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Luiseno", "lui", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lule Sami", "smj", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Lunda", "lun", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Luo", "luo", "", optn)                       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "lushai", "lus", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Luxembourgish", "ltz", "lb", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Letzeburgesch", "ltz", "lb", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Macedonian", "mac", "mk", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Macedonian", "mkd", "mk", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Madurese", "mad", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Magahi", "mag", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Maithili", "mai", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Makasar", "mak", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Malagasy", "mlg", "mg", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Malay", "may", "ms", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Malay", "msa", "ms", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Malayalam", "mal", "ml", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Maltese", "mlt", "mt", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Manchu", "mnc", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mandar", "mdr", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mandingo", "man", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Manipuri", "mni", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Manobo", "mno", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Manx", "glv", "gv", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Maori", "mao", "mi", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Maori", "mri", "mi", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Marathi", "mar", "mr", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mari", "chm", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Marshallese", "mah", "mh", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Marwari", "mwr", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Masai", "mas", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mayan", "myn", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mende", "men", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Micmac", "mic", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Minangkabau", "min", "", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mohawk", "moh", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Moksha", "mdf", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Moldavian", "mol", "mo", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mon-Khmer", "mkh", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mongo", "lol", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mongolian", "mon", "mn", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Mossi", "mos", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Multiple", "mul", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Munda", "mun", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nahuatl", "nah", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nauru", "nau", "na", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Navajo", "nav", "nv", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Navaho", "nav", "nv", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ndebele, North", "nde", "nd", optn)          | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "North Ndebele", "nde", "nd", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ndebele, South", "nbl", "nr", optn)          | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "South Ndebele", "nbl", "nr", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ndonga", "ndo", "ng", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Neapolitan", "nap", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nepali", "nep", "ne", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Newari", "new", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nepal Bhasa", "new", "", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nias", "nia", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Niger-Kordofanian", "nic", "", optn)         | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nilo-Saharan", "ssa", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Niuean", "niu", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nogai", "nog", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Norse, Old", "non", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "North American Indian", "nai", "", optn)     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Northern Sami", "sme", "se", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Norwegian", "nor", "no", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Norwegian Bokmål", "nob", "nb", optn)        | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Bokmål, Norwegian", "nob", "nb", optn)       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Norwegian Nynorsk", "nno", "nn", optn)       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nynorsk, Norwegian", "nno", "nn", optn)      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nubian", "nub", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nyamwezi", "nym", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nyankole", "nyn", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nyoro", "nyo", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Nzima", "nzi", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Occitan", "oci", "oc", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Provençal", "oci", "oc", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ojibwa", "oji", "oj", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Oriya", "ori", "or", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Oromo", "orm", "om", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Osage", "osa", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ossetian", "oss", "os", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ossetic", "oss", "os", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Otomian", "oto", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Pahlavi", "pal", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Palauan", "pau", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Pali", "pli", "pi", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Pampanga", "pam", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Pangasinan", "pag", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Panjabi", "pan", "pa", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Punjabi", "pan", "pa", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Papiamento", "pap", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Papuan", "paa", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Persian", "per", "fa", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Persian", "fas", "fa", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Persian, Old", "peo", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Philippine", "phi", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Phoenician", "phn", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Pohnpeian", "pon", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Polish", "pol", "pl", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Portuguese", "por", "pt", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Prakrit", "pra", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Provençal, Old", "pro", "", optn)            | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Pushto", "pus", "ps", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Quechua", "que", "qu", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Raeto-Romance", "roh", "rm", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Rajasthani", "raj", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Rapanui", "rap", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Rarotongan", "rar", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Romance", "roa", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Romanian", "rum", "ro", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Romanian", "ron", "ro", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Romany", "rom", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Rundi", "run", "rn", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Russian", "rus", "ru", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Salishan", "sal", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Samaritan Aramaic", "sam", "", optn)         | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sami", "smi", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Samoan", "smo", "sm", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sandawe", "sad", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sango", "sag", "sg", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sanskrit", "san", "sa", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Santali", "sat", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sardinian", "srd", "sc", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sasak", "sas", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Scots", "sco", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Selkup", "sel", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Semitic", "sem", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Serbian", "scc", "sr", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Serbian", "srp", "sr", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Serer", "srr", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Shan", "shn", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Shona", "sna", "sn", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sichuan Yi", "iii", "ii", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sidamo", "sid", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sign Languages", "sgn", "", optn)            | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Siksika", "bla", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sindhi", "snd", "sd", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sinhalese", "sin", "si", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sino-Tibetan", "sit", "", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Siouan", "sio", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Skolt Sami", "sms", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Slave", "den", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Athapascan", "den", "", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Slavic", "sla", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Slovak", "slo", "sk", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Slovak", "slk", "sk", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Slovenian", "slv", "sl", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sogdian", "sog", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Somali", "som", "so", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Songhai", "son", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Soninke", "snk", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sorbian", "wen", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sotho, Northern", "nso", "", optn)           | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sotho, Southern", "sot", "st", optn)         | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "South American Indian", "sai", "", optn)     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Southern Sami", "sma", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Spanish", "spa", "es", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Castilian", "spa", "es", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sukuma", "suk", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sumerian", "sux", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Sundanese", "sun", "su", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Susu", "sus", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Swahili", "swa", "sw", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Swati", "ssw", "ss", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Swedish", "swe", "sv", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Syriac", "syr", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tagalog", "tgl", "tl", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tahitian", "tah", "ty", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tai", "tai", "", optn)                       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tajik", "tgk", "tg", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tamashek", "tmh", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tamil", "tam", "ta", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tatar", "tat", "tt", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Telugu", "tel", "te", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tereno", "ter", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tetum", "tet", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Thai", "tha", "th", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tibetan", "tib", "bo", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tibetan", "bod", "bo", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tigre", "tig", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tigrinya", "tir", "ti", optn)                | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Timne", "tem", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tiv", "tiv", "", optn)                       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tlingit", "tli", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tok Pisin", "tpi", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tokelau", "tkl", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tonga (Nyasa)", "tog", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tonga (Tonga Islands)", "ton", "to", optn)   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tsimshian", "tsi", "", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tsonga", "tso", "ts", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tswana", "tsn", "tn", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tumbuka", "tum", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tupi", "tup", "", optn)                      | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Turkish", "tur", "tr", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Turkish, Ottoman", "ota", "", optn)          | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Turkmen", "tuk", "tk", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tuvalu", "tvl", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Tuvinian", "tyv", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Twi", "twi", "tw", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Udmurt", "udm", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ugaritic", "uga", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Uighur", "uig", "ug", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Ukrainian", "ukr", "uk", optn)               | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Umbundu", "umb", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Upper Sorbian", "hsb", "", optn)             | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Urdu", "urd", "ur", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Uzbek", "uzb", "uz", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Vai", "vai", "", optn)                       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Venda", "ven", "ve", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Vietnamese", "vie", "vi", optn)              | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Volapük", "vol", "vo", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Votic", "vot", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Wakashan", "wak", "", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Walamo", "wal", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Walloon", "wln", "wa", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Waray", "war", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Washo", "was", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Welsh", "wel", "cy", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Welsh", "cym", "cy", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Wolof", "wol", "wo", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Xhosa", "xho", "xh", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Yakut", "sah", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Yao", "yao", "", optn)                       | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Yapese", "yap", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Yiddish", "yid", "yi", optn)                 | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Yoruba", "yor", "yo", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Yupik", "ypk", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Zande", "znd", "", optn)                     | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Zapotec", "zap", "", optn)                   | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Zenaga", "zen", "", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Zhuang", "zha", "za", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Chuang", "zha", "za", optn)                  | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Zulu", "zul", "zu", optn)                    | if a != "0" | return a | endif
	let a = s:Cream_iso639_test(a:word, "Zuni", "zun", "", optn)                      | if a != "0" | return a | endif

	return 0

endfunction

" s:Cream_iso639_test() {{{1
function! s:Cream_iso639_test(word, name, abb3, abb2, optn)
" the brains of the operation  :)

	if     a:optn == ""
		if     a:word ==? a:name
			return a:abb3
		elseif a:word ==? a:abb3
			return a:name
		elseif a:word ==? a:abb2
			return a:name
		endif
	elseif a:optn == "name"
		if  a:word ==? a:name
		\|| a:word ==? a:abb3
		\|| a:word ==? a:abb2
			return a:name
		endif
	elseif a:optn == "3"
		if  a:word ==? a:name
		\|| a:word ==? a:abb3
		\|| a:word ==? a:abb2
			return a:abb3
		endif
	elseif a:optn == "2"
		if  a:word ==? a:name
		\|| a:word ==? a:abb3
		\|| a:word ==? a:abb2
			if a:abb2 != ""
				return a:abb2
			else
				return a:abb3
			endif
		endif
	endif

	return "0"

endfunction

" 1}}}
" vim:foldmethod=marker
