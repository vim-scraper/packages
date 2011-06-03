" WhatDomain
" Maintainer:	Michael Piefel <piefel@informatik.hu-berlin.de>
" Licence:	Public Domain
" Last Change:	2001 August 20

" This function returns the meaning of a Top Level Domain
" Usage: :call WhatDomain('de') will print 'DE: Germany'
function WhatDomain(...)
    if a:0 < 1
	let tld=input("Domain: ")
    else
	let tld=a:1
    endif
    let tld=toupper(tld)
    let TLD_INT ="International organisations"
    let TLD_EU  ="European Union (not yet)"
    let TLD_ORG ="(Non-profit) Organisations"
    let TLD_EDU ="Education (only US)"
    let TLD_NET ="Network"
    let TLD_COM ="Commercial (often US)"
    let TLD_GOV ="Government (only US)"
    let TLD_MIL ="Military (only US)"
    let TLD_ARPA="Advanced Research Projects Agency (US DoD)"
    let TLD_INFO="Information-based services"
    let TLD_BIZ ="Business and corporations"
    let TLD_NAME="Name (personal websites)"
    let TLD_PRO ="Professions"
    let TLD_COOP="Co-operative organisations"
    let TLD_AERO="Air travel"
    let TLD_MUSEUM="Museums"
    let TLD_KIDS="For children (not yet)"
    let TLD_SEX ="Adult entertainment (not yet)"
    " The following are ccTLD's, downloaded from IANA
    let TLD_AC="Ascension Island"
    let TLD_AD="Andorra"
    let TLD_AE="United Arab Emirates"
    let TLD_AF="Afghanistan"
    let TLD_AG="Antigua and Barbuda"
    let TLD_AI="Anguilla"
    let TLD_AL="Albania"
    let TLD_AM="Armenia"
    let TLD_AN="Netherlands Antilles"
    let TLD_AO="Angola"
    let TLD_AQ="Antartica"
    let TLD_AR="Argentina"
    let TLD_AS="American Samoa"
    let TLD_AT="Austria"
    let TLD_AU="Australia"
    let TLD_AW="Aruba"
    let TLD_AZ="Azerbaijan"
    let TLD_BA="Bosnia and Herzegovina"
    let TLD_BB="Barbados"
    let TLD_BD="Bangladesh"
    let TLD_BE="Belgium"
    let TLD_BF="Burkina Faso"
    let TLD_BG="Bulgaria"
    let TLD_BH="Bahrain"
    let TLD_BI="Burundi"
    let TLD_BJ="Benin"
    let TLD_BM="Bermuda"
    let TLD_BN="Brunei Darussalam"
    let TLD_BO="Bolivia"
    let TLD_BR="Brazil"
    let TLD_BS="Bahamas"
    let TLD_BT="Bhutan"
    let TLD_BV="Bouvet Island"
    let TLD_BW="Botswana"
    let TLD_BY="Belarus"
    let TLD_BZ="Belize"
    let TLD_CA="Canada"
    let TLD_CC="Cocos (Keeling) Islands"
    let TLD_CD="Congo, Democratic Republic of the"
    let TLD_CF="Central African Republic"
    let TLD_CG="Congo, Republic of"
    let TLD_CH="Switzerland"
    let TLD_CI="Cote d'Ivoire"
    let TLD_CK="Cook Islands"
    let TLD_CL="Chile"
    let TLD_CM="Cameroon"
    let TLD_CN="China"
    let TLD_CO="Colombia"
    let TLD_CR="Costa Rica"
    let TLD_CU="Cuba"
    let TLD_CV="Cap Verde"
    let TLD_CX="Christmas Island"
    let TLD_CY="Cyprus"
    let TLD_CZ="Czech Republic"
    let TLD_DE="Germany"
    let TLD_DJ="Djibouti"
    let TLD_DK="Denmark"
    let TLD_DM="Dominica"
    let TLD_DO="Dominican Republic"
    let TLD_DZ="Algeria"
    let TLD_EC="Ecuador"
    let TLD_EE="Estonia"
    let TLD_EG="Egypt"
    let TLD_EH="Western Sahara"
    let TLD_ER="Eritrea"
    let TLD_ES="Spain"
    let TLD_ET="Ethiopia"
    let TLD_FI="Finland"
    let TLD_FJ="Fiji"
    let TLD_FK="Falkland Islands (Malvina)"
    let TLD_FM="Micronesia, Federal State of"
    let TLD_FO="Faroe Islands"
    let TLD_FR="France"
    let TLD_GA="Gabon"
    let TLD_GD="Grenada"
    let TLD_GE="Georgia"
    let TLD_GF="French Guiana"
    let TLD_GG="Guernsey"
    let TLD_GH="Ghana"
    let TLD_GI="Gibraltar"
    let TLD_GL="Greenland"
    let TLD_GM="Gambia"
    let TLD_GN="Guinea"
    let TLD_GP="Guadeloupe"
    let TLD_GQ="Equatorial Guinea"
    let TLD_GR="Greece"
    let TLD_GS="South Georgia and the South Sandwich Islands"
    let TLD_GT="Guatemala"
    let TLD_GU="Guam"
    let TLD_GW="Guinea-Bissau"
    let TLD_GY="Guyana"
    let TLD_HK="Hong Kong"
    let TLD_HM="Heard and McDonald Islands"
    let TLD_HN="Honduras"
    let TLD_HR="Croatia/Hrvatska"
    let TLD_HT="Haiti"
    let TLD_HU="Hungary"
    let TLD_ID="Indonesia"
    let TLD_IE="Ireland"
    let TLD_IL="Israel"
    let TLD_IM="Isle of Man"
    let TLD_IN="India"
    let TLD_IO="British Indian Ocean Territory"
    let TLD_IQ="Iraq"
    let TLD_IR="Iran (Islamic Republic of)"
    let TLD_IS="Iceland"
    let TLD_IT="Italy"
    let TLD_JE="Jersey"
    let TLD_JM="Jamaica"
    let TLD_JO="Jordan"
    let TLD_JP="Japan"
    let TLD_KE="Kenya"
    let TLD_KG="Kyrgyzstan"
    let TLD_KH="Cambodia"
    let TLD_KI="Kiribati"
    let TLD_KM="Comoros"
    let TLD_KN="Saint Kitts and Nevis"
    let TLD_KP="Korea, Democratic People's Republic"
    let TLD_KR="Korea, Republic of"
    let TLD_KW="Kuwait"
    let TLD_KY="Cayman Islands"
    let TLD_KZ="Kazakhstan"
    let TLD_LA="Lao People's Democratic Republic"
    let TLD_LB="Lebanon"
    let TLD_LC="Saint Lucia"
    let TLD_LI="Liechtenstein"
    let TLD_LK="Sri Lanka"
    let TLD_LR="Liberia"
    let TLD_LS="Lesotho"
    let TLD_LT="Lithuania"
    let TLD_LU="Luxembourg"
    let TLD_LV="Latvia"
    let TLD_LY="Libyan Arab Jamahiriya"
    let TLD_MA="Morocco"
    let TLD_MC="Monaco"
    let TLD_MD="Moldova, Republic of"
    let TLD_MG="Madagascar"
    let TLD_MH="Marshall Islands"
    let TLD_MK="Macedonia, Former Yugoslav Republic"
    let TLD_ML="Mali"
    let TLD_MM="Myanmar"
    let TLD_MN="Mongolia"
    let TLD_MO="Macau"
    let TLD_MP="Northern Mariana Islands"
    let TLD_MQ="Martinique"
    let TLD_MR="Mauritania"
    let TLD_MS="Montserrat"
    let TLD_MT="Malta"
    let TLD_MU="Mauritius"
    let TLD_MV="Maldives"
    let TLD_MW="Malawi"
    let TLD_MX="Mexico"
    let TLD_MY="Malaysia"
    let TLD_MZ="Mozambique"
    let TLD_NA="Namibia"
    let TLD_NC="New Caledonia"
    let TLD_NE="Niger"
    let TLD_NF="Norfolk Island"
    let TLD_NG="Nigeria"
    let TLD_NI="Nicaragua"
    let TLD_NL="Netherlands"
    let TLD_NO="Norway"
    let TLD_NP="Nepal"
    let TLD_NR="Nauru"
    let TLD_NU="Niue"
    let TLD_NZ="New Zealand"
    let TLD_OM="Oman"
    let TLD_PA="Panama"
    let TLD_PE="Peru"
    let TLD_PF="French Polynesia"
    let TLD_PG="Papua New Guinea"
    let TLD_PH="Philippines"
    let TLD_PK="Pakistan"
    let TLD_PL="Poland"
    let TLD_PM="St. Pierre and Miquelon"
    let TLD_PN="Pitcairn Island"
    let TLD_PR="Puerto Rico"
    let TLD_PS="Palestinian Territories"
    let TLD_PT="Portugal"
    let TLD_PW="Palau"
    let TLD_PY="Paraguay"
    let TLD_QA="Qatar"
    let TLD_RE="Reunion Island"
    let TLD_RO="Romania"
    let TLD_RU="Russian Federation"
    let TLD_RW="Rwanda"
    let TLD_SA="Saudi Arabia"
    let TLD_SB="Solomon Islands"
    let TLD_SC="Seychelles"
    let TLD_SD="Sudan"
    let TLD_SE="Sweden"
    let TLD_SG="Singapore"
    let TLD_SH="St. Helena"
    let TLD_SI="Slovenia"
    let TLD_SJ="Svalbard and Jan Mayen Islands"
    let TLD_SK="Slovak Republic"
    let TLD_SL="Sierra Leone"
    let TLD_SM="San Marino"
    let TLD_SN="Senegal"
    let TLD_SO="Somalia"
    let TLD_SR="Suriname"
    let TLD_ST="Sao Tome and Principe"
    let TLD_SV="El Salvador"
    let TLD_SY="Syrian Arab Republic"
    let TLD_SZ="Swaziland"
    let TLD_TC="Turks and Caicos Islands"
    let TLD_TD="Chad"
    let TLD_TF="French Southern Territories"
    let TLD_TG="Togo"
    let TLD_TH="Thailand"
    let TLD_TJ="Tajikistan"
    let TLD_TK="Tokelau"
    let TLD_TM="Turkmenistan"
    let TLD_TN="Tunisia"
    let TLD_TO="Tonga"
    let TLD_TP="East Timor"
    let TLD_TR="Turkey"
    let TLD_TT="Trinidad and Tobago"
    let TLD_TV="Tuvalu"
    let TLD_TW="Taiwan"
    let TLD_TZ="Tanzania"
    let TLD_UA="Ukraine"
    let TLD_UG="Uganda"
    let TLD_UK="United Kingdom"
    let TLD_UM="US Minor Outlying Islands"
    let TLD_US="United States"
    let TLD_UY="Uruguay"
    let TLD_UZ="Uzbekistan"
    let TLD_VA="Holy See (City Vatican State)"
    let TLD_VC="Saint Vincent and the Grenadines"
    let TLD_VE="Venezuela"
    let TLD_VG="Virgin Islands (British)"
    let TLD_VI="Virgin Islands (USA)"
    let TLD_VN="Vietnam"
    let TLD_VU="Vanuatu"
    let TLD_WF="Wallis and Futuna Islands"
    let TLD_WS="Western Samoa"
    let TLD_YE="Yemen"
    let TLD_YT="Mayotte"
    let TLD_YU="Yugoslavia"
    let TLD_ZA="South Africa"
    let TLD_ZM="Zambia"
    let TLD_ZW="Zimbabwe"
    " Now do the work
    if exists("TLD_" . tld)
	echo "\r" . tld . ": " . TLD_{tld}
    else
	echohl WarningMsg | echo '\rUnknown domain' | echohl None
    endif
endfun
command -nargs=0 WhatDomain call WhatDomain()