{ Extract from Windows.pas for Linux }
(*
 *  Language IDs.
 *
 *  The following two combinations of primary language ID and
 *  sublanguage ID have special semantics:
 *
 *    Primary Language ID   Sublanguage ID      Result
 *    -------------------   ---------------     ------------------------
 *    LANG_NEUTRAL          SUBLANG_NEUTRAL     Language neutral
 *    LANG_NEUTRAL          SUBLANG_DEFAULT     User default language
 *    LANG_NEUTRAL          SUBLANG_SYS_DEFAULT System default language
 *)

const
{ Primary language IDs. }

  LANG_NEUTRAL                         = $00;
  LANG_AFRIKAANS                       = $36;
  LANG_ALBANIAN                        = $1c;
  LANG_ARABIC                          = $01;
  LANG_BASQUE                          = $2d;
  LANG_BELARUSIAN                      = $23;
  LANG_BULGARIAN                       = $02;
  LANG_CATALAN                         = $03;
  LANG_CHINESE                         = $04;
  LANG_CROATIAN                        = $1a;
  LANG_CZECH                           = $05;
  LANG_DANISH                          = $06;
  LANG_DUTCH                           = $13;
  LANG_ENGLISH                         = $09;
  LANG_ESTONIAN                        = $25;
  LANG_FAEROESE                        = $38;
  LANG_FARSI                           = $29;
  LANG_FINNISH                         = $0b;
  LANG_FRENCH                          = $0c;
  LANG_GERMAN                          = $07;
  LANG_GREEK                           = $08;
  LANG_HEBREW                          = $0d;
  LANG_HUNGARIAN                       = $0e;
  LANG_ICELANDIC                       = $0f;
  LANG_INDONESIAN                      = $21;
  LANG_ITALIAN                         = $10;
  LANG_JAPANESE                        = $11;
  LANG_KOREAN                          = $12;
  LANG_LATVIAN                         = $26;
  LANG_LITHUANIAN                      = $27;
  LANG_NORWEGIAN                       = $14;
  LANG_POLISH                          = $15;
  LANG_PORTUGUESE                      = $16;
  LANG_ROMANIAN                        = $18;
  LANG_RUSSIAN                         = $19;
  LANG_SERBIAN                         = $1a;
  LANG_SLOVAK                          = $1b;
  LANG_SLOVENIAN                       = $24;
  LANG_SPANISH                         = $0a;
  LANG_SWEDISH                         = $1d;
  LANG_THAI                            = $1e;
  LANG_TURKISH                         = $1f;
  LANG_UKRAINIAN                       = $22;
  LANG_VIETNAMESE                      = $2a;


{ Sublanguage IDs. }

  { The name immediately following SUBLANG_ dictates which primary
    language ID that sublanguage ID can be combined with to form a
    valid language ID. }

  SUBLANG_NEUTRAL                      = $00;    { language neutral }
  SUBLANG_DEFAULT                      = $01;    { user default }
  SUBLANG_SYS_DEFAULT                  = $02;    { system default }

  SUBLANG_ARABIC_SAUDI_ARABIA          = $01;    { Arabic (Saudi Arabia) }
  SUBLANG_ARABIC_IRAQ                  = $02;    { Arabic (Iraq) }
  SUBLANG_ARABIC_EGYPT                 = $03;    { Arabic (Egypt) }
  SUBLANG_ARABIC_LIBYA                 = $04;    { Arabic (Libya) }
  SUBLANG_ARABIC_ALGERIA               = $05;    { Arabic (Algeria) }
  SUBLANG_ARABIC_MOROCCO               = $06;    { Arabic (Morocco) }
  SUBLANG_ARABIC_TUNISIA               = $07;    { Arabic (Tunisia) }
  SUBLANG_ARABIC_OMAN                  = $08;    { Arabic (Oman) }
  SUBLANG_ARABIC_YEMEN                 = $09;    { Arabic (Yemen) }
  SUBLANG_ARABIC_SYRIA                 = $0a;    { Arabic (Syria) }
  SUBLANG_ARABIC_JORDAN                = $0b;    { Arabic (Jordan) }
  SUBLANG_ARABIC_LEBANON               = $0c;    { Arabic (Lebanon) }
  SUBLANG_ARABIC_KUWAIT                = $0d;    { Arabic (Kuwait) }
  SUBLANG_ARABIC_UAE                   = $0e;    { Arabic (U.A.E) }
  SUBLANG_ARABIC_BAHRAIN               = $0f;    { Arabic (Bahrain) }
  SUBLANG_ARABIC_QATAR                 = $10;    { Arabic (Qatar) }
  SUBLANG_CHINESE_TRADITIONAL          = $01;    { Chinese (Taiwan) }
  SUBLANG_CHINESE_SIMPLIFIED           = $02;    { Chinese (PR China) }
  SUBLANG_CHINESE_HONGKONG             = $03;    { Chinese (Hong Kong) }
  SUBLANG_CHINESE_SINGAPORE            = $04;    { Chinese (Singapore) }
  SUBLANG_DUTCH                        = $01;    { Dutch }
  SUBLANG_DUTCH_BELGIAN                = $02;    { Dutch (Belgian) }
  SUBLANG_ENGLISH_US                   = $01;    { English (USA) }
  SUBLANG_ENGLISH_UK                   = $02;    { English (UK) }
  SUBLANG_ENGLISH_AUS                  = $03;    { English (Australian) }
  SUBLANG_ENGLISH_CAN                  = $04;    { English (Canadian) }
  SUBLANG_ENGLISH_NZ                   = $05;    { English (New Zealand) }
  SUBLANG_ENGLISH_EIRE                 = $06;    { English (Irish) }
  SUBLANG_ENGLISH_SOUTH_AFRICA         = $07;    { English (South Africa) }
  SUBLANG_ENGLISH_JAMAICA              = $08;    { English (Jamaica) }
  SUBLANG_ENGLISH_CARIBBEAN            = $09;    { English (Caribbean) }
  SUBLANG_ENGLISH_BELIZE               = $0a;    { English (Belize) }
  SUBLANG_ENGLISH_TRINIDAD             = $0b;    { English (Trinidad) }
  SUBLANG_FRENCH                       = $01;    { French }
  SUBLANG_FRENCH_BELGIAN               = $02;    { French (Belgian) }
  SUBLANG_FRENCH_CANADIAN              = $03;    { French (Canadian) }
  SUBLANG_FRENCH_SWISS                 = $04;    { French (Swiss) }
  SUBLANG_FRENCH_LUXEMBOURG            = $05;    { French (Luxembourg) }
  SUBLANG_GERMAN                       = $01;    { German }
  SUBLANG_GERMAN_SWISS                 = $02;    { German (Swiss) }
  SUBLANG_GERMAN_AUSTRIAN              = $03;    { German (Austrian) }
  SUBLANG_GERMAN_LUXEMBOURG            = $04;    { German (Luxembourg) }
  SUBLANG_GERMAN_LIECHTENSTEIN         = $05;    { German (Liechtenstein) }
  SUBLANG_ITALIAN                      = $01;    { Italian }
  SUBLANG_ITALIAN_SWISS                = $02;    { Italian (Swiss) }
  SUBLANG_KOREAN                       = $01;    { Korean (Extended Wansung) }
  SUBLANG_KOREAN_JOHAB                 = $02;    { Korean (Johab) }
  SUBLANG_NORWEGIAN_BOKMAL             = $01;    { Norwegian (Bokmal) }
  SUBLANG_NORWEGIAN_NYNORSK            = $02;    { Norwegian (Nynorsk) }
  SUBLANG_PORTUGUESE                   = $02;    { Portuguese }
  SUBLANG_PORTUGUESE_BRAZILIAN         = $01;    { Portuguese (Brazilian) }
  SUBLANG_SERBIAN_LATIN                = $02;    { Serbian (Latin) }
  SUBLANG_SERBIAN_CYRILLIC             = $03;    { Serbian (Cyrillic) }
  SUBLANG_SPANISH                      = $01;    { Spanish (Castilian) }
  SUBLANG_SPANISH_MEXICAN              = $02;    { Spanish (Mexican) }
  SUBLANG_SPANISH_MODERN               = $03;    { Spanish (Modern) }
  SUBLANG_SPANISH_GUATEMALA            = $04;    { Spanish (Guatemala) }
  SUBLANG_SPANISH_COSTA_RICA           = $05;    { Spanish (Costa Rica) }
  SUBLANG_SPANISH_PANAMA               = $06;    { Spanish (Panama) }
  SUBLANG_SPANISH_DOMINICAN_REPUBLIC     = $07;  { Spanish (Dominican Republic) }
  SUBLANG_SPANISH_VENEZUELA            = $08;    { Spanish (Venezuela) }
  SUBLANG_SPANISH_COLOMBIA             = $09;    { Spanish (Colombia) }
  SUBLANG_SPANISH_PERU                 = $0a;    { Spanish (Peru) }
  SUBLANG_SPANISH_ARGENTINA            = $0b;    { Spanish (Argentina) }
  SUBLANG_SPANISH_ECUADOR              = $0c;    { Spanish (Ecuador) }
  SUBLANG_SPANISH_CHILE                = $0d;    { Spanish (Chile) }
  SUBLANG_SPANISH_URUGUAY              = $0e;    { Spanish (Uruguay) }
  SUBLANG_SPANISH_PARAGUAY             = $0f;    { Spanish (Paraguay) }
  SUBLANG_SPANISH_BOLIVIA              = $10;    { Spanish (Bolivia) }
  SUBLANG_SPANISH_EL_SALVADOR          = $11;    { Spanish (El Salvador) }
  SUBLANG_SPANISH_HONDURAS             = $12;    { Spanish (Honduras) }
  SUBLANG_SPANISH_NICARAGUA            = $13;    { Spanish (Nicaragua) }
  SUBLANG_SPANISH_PUERTO_RICO          = $14;    { Spanish (Puerto Rico) }
  SUBLANG_SWEDISH                      = $01;    { Swedish }
  SUBLANG_SWEDISH_FINLAND              = $02;    { Swedish (Finland) }


{ Sorting IDs. }

  SORT_DEFAULT                         = $0;     { sorting default }

  SORT_JAPANESE_XJIS                   = $0;     { Japanese XJIS order }
  SORT_JAPANESE_UNICODE                = $1;     { Japanese Unicode order }

  SORT_CHINESE_BIG5                    = $0;     { Chinese BIG5 order }
  SORT_CHINESE_PRCP                    = $0;     { PRC Chinese Phonetic order }
  SORT_CHINESE_UNICODE                 = $1;     { Chinese Unicode order }
  SORT_CHINESE_PRC                     = $2;     { PRC Chinese Stroke Count order }

  SORT_KOREAN_KSC                      = $0;     { Korean KSC order }
  SORT_KOREAN_UNICODE                  = $1;     { Korean Unicode order }

  SORT_GERMAN_PHONE_BOOK               = $1;     { German Phone Book order }


(*
 *  A language ID is a 16 bit value which is the combination of a
 *  primary language ID and a secondary language ID.  The bits are
 *  allocated as follows:
 *
 *       +-----------------------+-------------------------+
 *       |     Sublanguage ID    |   Primary Language ID   |
 *       +-----------------------+-------------------------+
 *        15                   10 9                       0   bit
 *
 *
 *
 *  A locale ID is a 32 bit value which is the combination of a
 *  language ID, a sort ID, and a reserved area.  The bits are
 *  allocated as follows:
 *
 *       +-------------+---------+-------------------------+
 *       |   Reserved  | Sort ID |      Language ID        |
 *       +-------------+---------+-------------------------+
 *        31         20 19     16 15                      0   bit
 *
 *)

{ Default System and User IDs for language and locale. }

  LANG_SYSTEM_DEFAULT   = (SUBLANG_SYS_DEFAULT shl 10) or LANG_NEUTRAL;
  LANG_USER_DEFAULT     = (SUBLANG_DEFAULT shl 10) or LANG_NEUTRAL;

  LOCALE_SYSTEM_DEFAULT = (SORT_DEFAULT shl 16) or LANG_SYSTEM_DEFAULT;
  LOCALE_USER_DEFAULT   = (SORT_DEFAULT shl 16) or LANG_USER_DEFAULT;

const
  LOCALE_SABBREVLANGNAME = $00000003;   { abbreviated language name }
