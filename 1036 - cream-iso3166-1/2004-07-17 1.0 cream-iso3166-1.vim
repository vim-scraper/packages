"=====================================================================
" cream-iso3166-1.vim -- Exchange ISO3166-1 country codes and names
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
" Updated:  2004-07-17 22:50:21-0400
"
" Description:
" Find ISO3166-1 country code or name when passed the other.
"
" Source:
"   http://en.wikipedia.org/wiki/ISO_3166-1
"
" Usage:
"
"   Cream_iso3166_1(word, ...)
"
" o Test {word} for match of iso3166_1-compliant country name, 3- or
"   2-letter abbreviation, or 3-digit numerical code:
"   * Returns name if {word} matches either abbreviation.
"   * Returns 2-letter if {word} matches name or number.
"   * Returns 0 if no match is made.
" o Use {optional} argument to force a given return:
"   * Returns 3-letter if {optional} is "3"      and {word} matches.
"   * Returns 2-letter if {optional} is "2"      and {word} matches.
"   * Returns name     if {optional} is "name"   and {word} matches.
"   * Returns number   if {optional} is "number" and {word} matches.
"   * Returns 0 if no match is made.
" o Matching is case-insensitive. But return values are capitalized
"   according to the standard. (Name is title case, abbreviations are
"   UPPER CASE.)
"
" Examples:
"
"   :echo Cream_iso3166_1("AND")                returns "Andorra"
"   :echo Cream_iso3166_1("AD")                 returns "Andorra"
"   :echo Cream_iso3166_1("Andorra")            returns "AD"
"   :echo Cream_iso3166_1("020")                returns "AD"
"
" Name or abbreviation unmatched:
"
"   :echo Cream_iso3166_1("not-a-name")         returns 0
"   :echo Cream_iso3166_1("not")                returns 0
"

" Cream_iso3166_1() {{{1
function! Cream_iso3166_1(word, ...)
" test strings

	if a:0 > 0
		if  a:1 == "2"
		\|| a:1 == "3"
		\|| a:1 == "number"
		\|| a:1 == "name"
			let optn = a:1
		else
			let optn = ""
		endif
	else
		let optn = ""
	endif

	let a = s:Cream_iso3166_1_test(a:word, "004", "AFG", "AF", "Afghanistan", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "248", "ALA", "AX", "Åland Islands", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "008", "ALB", "AL", "Albania", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "012", "DZA", "DZ", "Algeria", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "016", "ASM", "AS", "American Samoa", optn)                   | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "020", "AND", "AD", "Andorra", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "024", "AGO", "AO", "Angola", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "660", "AIA", "AI", "Anguilla", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "010", "ATA", "AQ", "Antarctica", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "028", "ATG", "AG", "Antigua and Barbuda", optn)              | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "032", "ARG", "AR", "Argentina", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "051", "ARM", "AM", "Armenia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "533", "ABW", "AW", "Aruba", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "036", "AUS", "AU", "Australia", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "040", "AUT", "AT", "Austria", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "031", "AZE", "AZ", "Azerbaijan", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "044", "BHS", "BS", "Bahamas", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "048", "BHR", "BH", "Bahrain", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "050", "BGD", "BD", "Bangladesh", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "052", "BRB", "BB", "Barbados", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "112", "BLR", "BY", "Belarus", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "056", "BEL", "BE", "Belgium", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "084", "BLZ", "BZ", "Belize", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "204", "BEN", "BJ", "Benin", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "060", "BMU", "BM", "Bermuda", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "064", "BTN", "BT", "Bhutan", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "068", "BOL", "BO", "Bolivia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "070", "BIH", "BA", "Bosnia and Herzegovina", optn)           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "072", "BWA", "BW", "Botswana", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "074", "BVT", "BV", "Bouvet Island", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "076", "BRA", "BR", "Brazil", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "086", "IOT", "IO", "British Indian Ocean Territory", optn)   | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "096", "BRN", "BN", "Brunei Darussalam", optn)                | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "100", "BGR", "BG", "Bulgaria", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "854", "BFA", "BF", "Burkina Faso", optn)                     | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "108", "BDI", "BI", "Burundi", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "116", "KHM", "KH", "Cambodia", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "120", "CMR", "CM", "Cameroon", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "124", "CAN", "CA", "Canada", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "132", "CPV", "CV", "Cape Verde", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "136", "CYM", "KY", "Cayman Islands", optn)                   | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "140", "CAF", "CF", "Central African Republic", optn)         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "148", "TCD", "TD", "Chad", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "152", "CHL", "CL", "Chile", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "156", "CHN", "CN", "China, mainland", optn)                  | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "162", "CXR", "CX", "Christmas Island", optn)                 | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "166", "CCK", "CC", "Cocos (Keeling) Islands", optn)          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "170", "COL", "CO", "Colombia", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "174", "COM", "KM", "Comoros", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "178", "COG", "CG", "Congo, Republic of the ", optn)          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "180", "COD", "CD", "Congo, The Democratic Republic Of The", optn) | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "184", "COK", "CK", "Cook Islands", optn)                     | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "188", "CRI", "CR", "Costa Rica", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "384", "CIV", "CI", "Côte d'Ivoire", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "191", "HRV", "HR", "Croatia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "192", "CUB", "CU", "Cuba", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "196", "CYP", "CY", "Cyprus", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "203", "CZE", "CZ", "Czech Republic", optn)                   | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "208", "DNK", "DK", "Denmark", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "262", "DJI", "DJ", "Djibouti", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "212", "DMA", "DM", "Dominica", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "214", "DOM", "DO", "Dominican Republic", optn)               | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "218", "ECU", "EC", "Ecuador", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "818", "EGY", "EG", "Egypt", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "222", "SLV", "SV", "El Salvador", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "226", "GNQ", "GQ", "Equatorial Guinea", optn)                | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "232", "ERI", "ER", "Eritrea", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "233", "EST", "EE", "Estonia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "231", "ETH", "ET", "Ethiopia", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "238", "FLK", "FK", "Falkland Islands", optn)                 | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "234", "FRO", "FO", "Faroe Islands", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "242", "FJI", "FJ", "Fiji", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "246", "FIN", "FI", "Finland", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "250", "FRA", "FR", "France", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "254", "GUF", "GF", "French Guiana", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "258", "PYF", "PF", "French Polynesia", optn)                 | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "260", "ATF", "TF", "French Southern Territories", optn)      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "266", "GAB", "GA", "Gabon", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "270", "GMB", "GM", "Gambia", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "268", "GEO", "GE", "Georgia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "276", "DEU", "DE", "Germany", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "288", "GHA", "GH", "Ghana", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "292", "GIB", "GI", "Gibraltar", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "300", "GRC", "GR", "Greece", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "304", "GRL", "GL", "Greenland", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "308", "GRD", "GD", "Grenada", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "312", "GLP", "GP", "Guadeloupe", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "316", "GUM", "GU", "Guam", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "320", "GTM", "GT", "Guatemala", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "324", "GIN", "GN", "Guinea", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "624", "GNB", "GW", "Guinea-Bissau", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "328", "GUY", "GY", "Guyana", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "332", "HTI", "HT", "Haiti", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "334", "HMD", "HM", "Heard Island and McDonald Islands", optn)| if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "336", "VAT", "VA", "Vatican City State", optn)               | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "340", "HND", "HN", "Honduras", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "344", "HKG", "HK", "Hong Kong", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "348", "HUN", "HU", "Hungary", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "352", "ISL", "IS", "Iceland", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "356", "IND", "IN", "India", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "360", "IDN", "ID", "Indonesia", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "364", "IRN", "IR", "Iran, Islamic Republic of", optn)        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "368", "IRQ", "IQ", "Iraq", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "372", "IRL", "IE", "Ireland, Republic of", optn)             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "376", "ISR", "IL", "Israel", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "380", "ITA", "IT", "Italy", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "388", "JAM", "JM", "Jamaica", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "392", "JPN", "JP", "Japan", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "400", "JOR", "JO", "Jordan", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "398", "KAZ", "KZ", "Kazakhstan", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "404", "KEN", "KE", "Kenya", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "296", "KIR", "KI", "Kiribati", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "408", "PRK", "KP", "Korea, Democratic People's Republic of", optn) | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "410", "KOR", "KR", "Korea, Republic of", optn)               | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "414", "KWT", "KW", "Kuwait", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "417", "KGZ", "KG", "Kyrgyzstan", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "418", "LAO", "LA", "Lao People's Democratic Republic", optn) | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "428", "LVA", "LV", "Latvia", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "422", "LBN", "LB", "Lebanon", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "426", "LSO", "LS", "Lesotho", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "430", "LBR", "LR", "Liberia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "434", "LBY", "LY", "Libyan Arab Jamahiriya", optn)           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "438", "LIE", "LI", "Liechtenstein", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "440", "LTU", "LT", "Lithuania", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "442", "LUX", "LU", "Luxembourg", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "446", "MAC", "MO", "Macao", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "807", "MKD", "MK", "Macedonia, The Former Yugoslav Republic of", optn) | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "450", "MDG", "MG", "Madagascar", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "454", "MWI", "MW", "Malawi", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "458", "MYS", "MY", "Malaysia", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "462", "MDV", "MV", "Maldives", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "466", "MLI", "ML", "Mali", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "470", "MLT", "MT", "Malta", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "584", "MHL", "MH", "Marshall Islands", optn)                 | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "474", "MTQ", "MQ", "Martinique", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "478", "MRT", "MR", "Mauritania", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "480", "MUS", "MU", "Mauritius", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "175", "MYT", "YT", "Mayotte", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "484", "MEX", "MX", "Mexico", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "583", "FSM", "FM", "Micronesia, Federated States of", optn)  | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "498", "MDA", "MD", "Moldova, Republic of", optn)             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "492", "MCO", "MC", "Monaco", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "496", "MNG", "MN", "Mongolia", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "500", "MSR", "MS", "Montserrat", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "504", "MAR", "MA", "Morocco", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "508", "MOZ", "MZ", "Mozambique", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "104", "MMR", "MM", "Myanmar", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "516", "NAM", "NA", "Namibia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "520", "NRU", "NR", "Nauru", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "524", "NPL", "NP", "Nepal", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "528", "NLD", "NL", "Netherlands", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "530", "ANT", "AN", "Netherlands Antilles", optn)             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "540", "NCL", "NC", "New Caledonia", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "554", "NZL", "NZ", "New Zealand", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "558", "NIC", "NI", "Nicaragua", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "562", "NER", "NE", "Niger", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "566", "NGA", "NG", "Nigeria", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "570", "NIU", "NU", "Niue", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "574", "NFK", "NF", "Norfolk Island", optn)                   | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "580", "MNP", "MP", "Northern Mariana Islands", optn)         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "578", "NOR", "NO", "Norway", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "512", "OMN", "OM", "Oman", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "586", "PAK", "PK", "Pakistan", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "585", "PLW", "PW", "Palau", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "275", "PSE", "PS", "Palestinian Territory, Occupied", optn)  | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "591", "PAN", "PA", "Panama", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "598", "PNG", "PG", "Papua New Guinea", optn)                 | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "600", "PRY", "PY", "Paraguay", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "604", "PER", "PE", "Peru", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "608", "PHL", "PH", "Philippines", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "612", "PCN", "PN", "Pitcairn", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "616", "POL", "PL", "Poland", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "620", "PRT", "PT", "Portugal", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "630", "PRI", "PR", "Puerto Rico", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "634", "QAT", "QA", "Qatar", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "638", "REU", "RE", "Réunion", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "642", "ROU", "RO", "Romania", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "643", "RUS", "RU", "Russian Federation", optn)               | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "646", "RWA", "RW", "Rwanda", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "654", "SHN", "SH", "Saint Helena", optn)                     | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "659", "KNA", "KN", "Saint Kitts and Nevis", optn)            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "662", "LCA", "LC", "Saint Lucia", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "666", "SPM", "PM", "Saint-Pierre and Miquelon", optn)        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "670", "VCT", "VC", "Saint Vincent and the Grenadines", optn) | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "882", "WSM", "WS", "Samoa", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "674", "SMR", "SM", "San Marino", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "678", "STP", "ST", "São Tomé and Príncipe", optn)            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "682", "SAU", "SA", "Saudi Arabia", optn)                     | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "686", "SEN", "SN", "Senegal", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "891", "SCG", "CS", "Serbia and Montenegro", optn)            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "690", "SYC", "SC", "Seychelles", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "694", "SLE", "SL", "Sierra Leone", optn)                     | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "702", "SGP", "SG", "Singapore", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "703", "SVK", "SK", "Slovakia", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "705", "SVN", "SI", "Slovenia", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "090", "SLB", "SB", "Solomon Islands", optn)                  | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "706", "SOM", "SO", "Somalia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "710", "ZAF", "ZA", "South Africa", optn)                     | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "239", "SGS", "GS", "South Georgia and the South Sandwich Islands", optn) | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "724", "ESP", "ES", "Spain", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "144", "LKA", "LK", "Sri Lanka", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "736", "SDN", "SD", "Sudan", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "740", "SUR", "SR", "Suriname", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "744", "SJM", "SJ", "Svalbard and Jan Mayen", optn)           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "748", "SWZ", "SZ", "Swaziland", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "752", "SWE", "SE", "Sweden", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "756", "CHE", "CH", "Switzerland", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "760", "SYR", "SY", "Syrian Arab Republic", optn)             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "158", "TWN", "TW", "Taiwan (Republic of China)", optn)       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "762", "TJK", "TJ", "Tajikistan", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "834", "TZA", "TZ", "Tanzania, United Republic Of", optn)     | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "764", "THA", "TH", "Thailand", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "626", "TLS", "TL", "Timor-Leste", optn)                      | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "768", "TGO", "TG", "Togo", optn)                             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "772", "TKL", "TK", "Tokelau", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "776", "TON", "TO", "Tonga", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "780", "TTO", "TT", "Trinidad and Tobago", optn)              | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "788", "TUN", "TN", "Tunisia", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "792", "TUR", "TR", "Turkey", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "795", "TKM", "TM", "Turkmenistan", optn)                     | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "796", "TCA", "TC", "Turks and Caicos Islands", optn)         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "798", "TUV", "TV", "Tuvalu", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "800", "UGA", "UG", "Uganda", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "804", "UKR", "UA", "Ukraine", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "784", "ARE", "AE", "United Arab Emirates", optn)             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "826", "GBR", "GB", "United Kingdom", optn)                   | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "840", "USA", "US", "United States", optn)                    | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "581", "UMI", "UM", "United States Minor Outlying Islands", optn) | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "858", "URY", "UY", "Uruguay", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "860", "UZB", "UZ", "Uzbekistan", optn)                       | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "548", "VUT", "VU", "Vanuatu", optn)                          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "862", "VEN", "VE", "Venezuela", optn)                        | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "704", "VNM", "VN", "Viet Nam", optn)                         | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "092", "VGB", "VG", "Virgin Islands, British", optn)          | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "850", "VIR", "VI", "Virgin Islands, U.S.", optn)             | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "876", "WLF", "WF", "Wallis and Futuna", optn)                | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "732", "ESH", "EH", "Western Sahara", optn)                   | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "887", "YEM", "YE", "Yemen", optn)                            | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "894", "ZMB", "ZM", "Zambia", optn)                           | if a!="0" | return a | endif
	let a = s:Cream_iso3166_1_test(a:word, "716", "ZWE", "ZW", "Zimbabwe", optn)                         | if a!="0" | return a | endif

	return 0

endfunction

" s:Cream_iso3166_1_test() {{{1
function! s:Cream_iso3166_1_test(word, numb, abb3, abb2, name, optn)
" the brains of the operation  :)

	if     a:optn == ""
		if     a:word ==? a:abb2
			return a:name
		elseif a:word ==? a:abb3
			return a:name
		elseif a:word ==? a:numb
			return a:abb2
		elseif a:word ==? a:name
			return a:abb2
		endif
	elseif a:optn == "number"
		if  a:word ==? a:name
		\|| a:word ==? a:numb
		\|| a:word ==? a:abb3
		\|| a:word ==? a:abb2
			return a:numb
		endif
	elseif a:optn == "name"
		if  a:word ==? a:name
		\|| a:word ==? a:numb
		\|| a:word ==? a:abb3
		\|| a:word ==? a:abb2
			return a:name
		endif
	elseif a:optn == "3"
		if  a:word ==? a:name
		\|| a:word ==? a:numb
		\|| a:word ==? a:abb3
		\|| a:word ==? a:abb2
			return a:abb3
		endif
	elseif a:optn == "2"
		if  a:word ==? a:name
		\|| a:word ==? a:numb
		\|| a:word ==? a:abb3
		\|| a:word ==? a:abb2
			return a:abb2
		endif
	endif

	return "0"

endfunction

" 1}}}
" vim:foldmethod=marker
