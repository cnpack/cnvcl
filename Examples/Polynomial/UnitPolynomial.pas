unit UnitPolynomial;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Contnrs, CnPolynomial, CnECC, CnBigNumber;

type
  TFormPolynomial = class(TForm)
    pgcPoly: TPageControl;
    tsIntegerPolynomial: TTabSheet;
    grpIntegerPolynomial: TGroupBox;
    btnIPCreate: TButton;
    edtIP1: TEdit;
    bvl1: TBevel;
    mmoIP1: TMemo;
    mmoIP2: TMemo;
    btnIP1Random: TButton;
    btnIP2Random: TButton;
    lblDeg1: TLabel;
    lblDeg2: TLabel;
    edtIPDeg1: TEdit;
    edtIPDeg2: TEdit;
    btnIPAdd: TButton;
    btnIPSub: TButton;
    btnIPMul: TButton;
    btnIPDiv: TButton;
    lblIPEqual: TLabel;
    edtIP3: TEdit;
    btnTestExample1: TButton;
    btnTestExample2: TButton;
    bvl2: TBevel;
    btnTestExample3: TButton;
    btnTestExample4: TButton;
    tsExtensionEcc: TTabSheet;
    grpEccGalois: TGroupBox;
    btnGaloisOnCurve: TButton;
    btnPolyGcd: TButton;
    btnGaloisTestGcd: TButton;
    btnTestGaloisMI: TButton;
    btnGF28Test1: TButton;
    btnEccPointAdd: TButton;
    btnTestEccPointAdd2: TButton;
    btnTestDivPoly: TButton;
    btnTestDivPoly2: TButton;
    btnTestGaloisPoint2: TButton;
    btnTestPolyPoint2: TButton;
    btnTestPolyEccPoint3: TButton;
    btnTestGaloisPolyMulMod: TButton;
    btnTestGaloisModularInverse1: TButton;
    btnTestEuclid2: TButton;
    btnTestExtendEuclid3: TButton;
    btnTestGaloisDiv: TButton;
    btnTestEccDivisionPoly3: TButton;
    mmoTestDivisionPolynomial: TMemo;
    btnGenerateDivisionPolynomial: TButton;
    tsRationalPolynomial: TTabSheet;
    grpRationalPolynomial: TGroupBox;
    btnRP2Point: TButton;
    bvl3: TBevel;
    edtRationalNominator1: TEdit;
    lbl1: TLabel;
    edtRationalDenominator1: TEdit;
    btnRationalPolynomialAdd: TButton;
    btnRationalPolynomialSub: TButton;
    btnRationalPolynomialMul: TButton;
    btnRationalPolynomialDiv: TButton;
    chkRationalPolynomialGalois: TCheckBox;
    edtRationalPolynomialPrime: TEdit;
    edtRationalNominator2: TEdit;
    lbl2: TLabel;
    edtRationalDenominator2: TEdit;
    lbl3: TLabel;
    lbl4: TLabel;
    btnRationalPolynomialGenerate: TButton;
    edtRationalResultNominator: TEdit;
    edtRationalResultDenominator: TEdit;
    btnManualOnCurve: TButton;
    btnCheckDivisionPolynomialZero: TButton;
    btnCalcSimpleEcc: TButton;
    mmoEcc: TMemo;
    bvl4: TBevel;
    btnCheckRationalAdd: TButton;
    btnTestPiXPolynomial: TButton;
    btnTestGaloisDivTime: TButton;
    btnTestGaloisCalc: TButton;
    btnTestGaloisEqual: TButton;
    btnTestHugeDiv: TButton;
    btnTestHugeDiv2: TButton;
    btnTestHugeDiv3: TButton;
    btnTestPowerMod: TButton;
    tsBNPolynomial: TTabSheet;
    grpBNPolynomial: TGroupBox;
    btnBNPToString: TButton;
    edtBNPolynomial: TEdit;
    mmoBP1: TMemo;
    mmoBP2: TMemo;
    btnBPAdd: TButton;
    btnBPSub: TButton;
    btnBPMul: TButton;
    btnBPDivMod: TButton;
    lblBPEqual: TLabel;
    edtBP3: TEdit;
    lblBP1Deg: TLabel;
    edtBP2Deg: TEdit;
    btnBP1Rand: TButton;
    lblBP2Deg: TLabel;
    edtBP1Deg: TEdit;
    btnBP2Rand: TButton;
    btnBNTestExample1: TButton;
    btnBNTestExample2: TButton;
    btnBNTestExample3: TButton;
    btnBNTestExample4: TButton;
    btnBNGaloisTestGcd: TButton;
    btnBNTestGaloisMI: TButton;
    btnBNPolyGcd: TButton;
    btnBNGF28Test1: TButton;
    btnBNTestGaloisDiv: TButton;
    btnBNTestGaloisCalc: TButton;
    btnBNTestHugeDiv1: TButton;
    btnBNTestHugeDiv2: TButton;
    btnBNTestHugeDiv3: TButton;
    btnBNTestPowerMod: TButton;
    btnBNTestGaloisDivTime: TButton;
    bvl5: TBevel;
    tsBNEccOnGalois: TTabSheet;
    grp1: TGroupBox;
    btnBNTestMI1: TButton;
    btnBNTestEuclid2: TButton;
    btnBNTestGaloisMulMod: TButton;
    btnBNTestEuclid3: TButton;
    btnBNEccDivisionPoly: TButton;
    btnBNGenerateDP: TButton;
    mmoBNTestDivisionPolynomials: TMemo;
    btnBNTestDivPoly1: TButton;
    btnBNTestDivPoly2: TButton;
    tsBNRationalPolynomial: TTabSheet;
    grpBNRationalPolynomial: TGroupBox;
    btnBNTestDivPoly: TButton;
    bvl6: TBevel;
    btnBNEccOnCurve: TButton;
    btnBNEccPointAdd1: TButton;
    btnBNEccPointAdd2: TButton;
    btnBNTestPoly1: TButton;
    btnBNTestPoly2: TButton;
    btnBNTestPoly3: TButton;
    btnBNTestPointAdd: TButton;
    btnBNTestManualPoint: TButton;
    lbl5: TLabel;
    edtBNRationalNominator1: TEdit;
    edtBNRationalDenominator1: TEdit;
    btnBNRationalGenerate: TButton;
    btnBNRationalAdd: TButton;
    btnBNRationalSub: TButton;
    btnBNRationalMul: TButton;
    btnBNRationalDiv: TButton;
    chkBNRationalGalois: TCheckBox;
    edtBNRationalGalois: TEdit;
    edtBNRationalNominator2: TEdit;
    edtBNRationalDenominator2: TEdit;
    lbl6: TLabel;
    lbl7: TLabel;
    edtBNRationalResultNominator: TEdit;
    edtBNRationalResultDenominator: TEdit;
    bvl7: TBevel;
    btnBNTestRationalPointAdd1: TButton;
    btnBNTestRationalPointAdd2: TButton;
    btnBNTestGaloisEqual: TButton;
    btnInt64PolySetString: TButton;
    btnBNPolySetString: TButton;
    btnRationalSetString: TButton;
    btnBNRationalSetString: TButton;
    btnTestBigDiv: TButton;
    btnTestBigGCD: TButton;
    btnInt64ComposeRationalRational: TButton;
    btnInt64ComposePolyRational: TButton;
    btnInt64ComposeRationalPolynomial: TButton;
    btnBNRationalRational: TButton;
    btnBNPolyRational: TButton;
    btnBNRationalPoly: TButton;
    btnCompareRationalMul2Method: TButton;
    btnInt64MulDFT: TButton;
    btnTestInt64SimpleDFT: TButton;
    btnTestInt64SimpleNTT: TButton;
    tsBiInt64Polynomial: TTabSheet;
    grpBiInt64Poly: TGroupBox;
    btnBiInt64ToString: TButton;
    edtBIP: TEdit;
    btnBiInt64SetString: TButton;
    mmoIBP1: TMemo;
    btnIBP1Random: TButton;
    lblIBPDeg: TLabel;
    edtIBP1Deg: TEdit;
    mmoIBP2: TMemo;
    btnIBP2Rand: TButton;
    edtIBP2Deg: TEdit;
    lblIBP2Deg: TLabel;
    btnIBPAdd: TButton;
    btnIBPSub: TButton;
    btnIBPMul: TButton;
    lblIBPEqual: TLabel;
    edtIBP3: TEdit;
    btnIBPPower: TButton;
    lblIBPPower: TLabel;
    edtIBPPower: TEdit;
    btnIBPEvalY: TButton;
    btnIBPEvalX: TButton;
    btnIBPTranspose: TButton;
    bvl8: TBevel;
    bvl9: TBevel;
    lblIBPExtract: TLabel;
    edtIBPExtract: TEdit;
    btnIBPExtractXY: TButton;
    btnIBPIsMonicX: TButton;
    btnIPIsMonic: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnIPCreateClick(Sender: TObject);
    procedure btnIP1RandomClick(Sender: TObject);
    procedure btnIP2RandomClick(Sender: TObject);
    procedure btnIPAddClick(Sender: TObject);
    procedure btnIPSubClick(Sender: TObject);
    procedure btnIPMulClick(Sender: TObject);
    procedure btnIPDivClick(Sender: TObject);
    procedure btnTestExample1Click(Sender: TObject);
    procedure btnTestExample2Click(Sender: TObject);
    procedure btnTestExample3Click(Sender: TObject);
    procedure btnTestExample4Click(Sender: TObject);
    procedure btnGaloisOnCurveClick(Sender: TObject);
    procedure btnPolyGcdClick(Sender: TObject);
    procedure btnGaloisTestGcdClick(Sender: TObject);
    procedure btnTestGaloisMIClick(Sender: TObject);
    procedure btnGF28Test1Click(Sender: TObject);
    procedure btnEccPointAddClick(Sender: TObject);
    procedure btnTestEccPointAdd2Click(Sender: TObject);
    procedure btnTestDivPolyClick(Sender: TObject);
    procedure btnTestDivPoly2Click(Sender: TObject);
    procedure btnTestGaloisPoint2Click(Sender: TObject);
    procedure btnTestPolyPoint2Click(Sender: TObject);
    procedure btnTestPolyEccPoint3Click(Sender: TObject);
    procedure btnTestGaloisPolyMulModClick(Sender: TObject);
    procedure btnTestGaloisModularInverse1Click(Sender: TObject);
    procedure btnTestEuclid2Click(Sender: TObject);
    procedure btnTestExtendEuclid3Click(Sender: TObject);
    procedure btnTestGaloisDivClick(Sender: TObject);
    procedure btnTestEccDivisionPoly3Click(Sender: TObject);
    procedure btnGenerateDivisionPolynomialClick(Sender: TObject);
    procedure btnRP2PointClick(Sender: TObject);
    procedure btnRationalPolynomialGenerateClick(Sender: TObject);
    procedure btnRationalPolynomialAddClick(Sender: TObject);
    procedure btnRationalPolynomialSubClick(Sender: TObject);
    procedure btnRationalPolynomialMulClick(Sender: TObject);
    procedure btnRationalPolynomialDivClick(Sender: TObject);
    procedure btnManualOnCurveClick(Sender: TObject);
    procedure btnCheckDivisionPolynomialZeroClick(Sender: TObject);
    procedure btnCalcSimpleEccClick(Sender: TObject);
    procedure btnCheckRationalAddClick(Sender: TObject);
    procedure btnTestPiXPolynomialClick(Sender: TObject);
    procedure btnTestGaloisDivTimeClick(Sender: TObject);
    procedure btnTestGaloisCalcClick(Sender: TObject);
    procedure btnTestGaloisEqualClick(Sender: TObject);
    procedure btnTestHugeDivClick(Sender: TObject);
    procedure btnTestHugeDiv2Click(Sender: TObject);
    procedure btnTestHugeDiv3Click(Sender: TObject);
    procedure btnTestPowerModClick(Sender: TObject);
    procedure btnBNPToStringClick(Sender: TObject);
    procedure btnBP2RandClick(Sender: TObject);
    procedure btnBP1RandClick(Sender: TObject);
    procedure btnBPAddClick(Sender: TObject);
    procedure btnBPSubClick(Sender: TObject);
    procedure btnBPMulClick(Sender: TObject);
    procedure btnBPDivModClick(Sender: TObject);
    procedure btnBNTestExample1Click(Sender: TObject);
    procedure btnBNTestExample2Click(Sender: TObject);
    procedure btnBNTestExample3Click(Sender: TObject);
    procedure btnBNTestExample4Click(Sender: TObject);
    procedure btnBNGaloisTestGcdClick(Sender: TObject);
    procedure btnBNTestGaloisMIClick(Sender: TObject);
    procedure btnBNGF28Test1Click(Sender: TObject);
    procedure btnBNPolyGcdClick(Sender: TObject);
    procedure btnBNTestGaloisDivClick(Sender: TObject);
    procedure btnBNTestGaloisCalcClick(Sender: TObject);
    procedure btnBNTestHugeDiv1Click(Sender: TObject);
    procedure btnBNTestHugeDiv2Click(Sender: TObject);
    procedure btnBNTestHugeDiv3Click(Sender: TObject);
    procedure btnBNTestPowerModClick(Sender: TObject);
    procedure btnBNTestGaloisDivTimeClick(Sender: TObject);
    procedure btnBNTestMI1Click(Sender: TObject);
    procedure btnBNTestEuclid2Click(Sender: TObject);
    procedure btnBNTestGaloisMulModClick(Sender: TObject);
    procedure btnBNTestEuclid3Click(Sender: TObject);
    procedure btnBNEccDivisionPolyClick(Sender: TObject);
    procedure btnBNGenerateDPClick(Sender: TObject);
    procedure btnBNTestDivPoly1Click(Sender: TObject);
    procedure btnBNTestDivPoly2Click(Sender: TObject);
    procedure btnBNTestDivPolyClick(Sender: TObject);
    procedure btnBNEccOnCurveClick(Sender: TObject);
    procedure btnBNEccPointAdd1Click(Sender: TObject);
    procedure btnBNEccPointAdd2Click(Sender: TObject);
    procedure btnBNTestPoly1Click(Sender: TObject);
    procedure btnBNTestPoly2Click(Sender: TObject);
    procedure btnBNTestPoly3Click(Sender: TObject);
    procedure btnBNTestPointAddClick(Sender: TObject);
    procedure btnBNTestManualPointClick(Sender: TObject);
    procedure btnBNRationalGenerateClick(Sender: TObject);
    procedure btnBNRationalAddClick(Sender: TObject);
    procedure btnBNRationalSubClick(Sender: TObject);
    procedure btnBNRationalMulClick(Sender: TObject);
    procedure btnBNRationalDivClick(Sender: TObject);
    procedure btnBNTestRationalPointAdd1Click(Sender: TObject);
    procedure btnBNTestRationalPointAdd2Click(Sender: TObject);
    procedure btnBNTestGaloisEqualClick(Sender: TObject);
    procedure btnInt64PolySetStringClick(Sender: TObject);
    procedure btnBNPolySetStringClick(Sender: TObject);
    procedure btnRationalSetStringClick(Sender: TObject);
    procedure btnBNRationalSetStringClick(Sender: TObject);
    procedure btnTestBigDivClick(Sender: TObject);
    procedure btnTestBigGCDClick(Sender: TObject);
    procedure btnInt64ComposeRationalPolynomialClick(Sender: TObject);
    procedure btnInt64ComposeRationalRationalClick(Sender: TObject);
    procedure btnInt64ComposePolyRationalClick(Sender: TObject);
    procedure btnBNRationalRationalClick(Sender: TObject);
    procedure btnBNPolyRationalClick(Sender: TObject);
    procedure btnBNRationalPolyClick(Sender: TObject);
    procedure btnCompareRationalMul2MethodClick(Sender: TObject);
    procedure btnInt64MulDFTClick(Sender: TObject);
    procedure btnTestInt64SimpleDFTClick(Sender: TObject);
    procedure btnTestInt64SimpleNTTClick(Sender: TObject);
    procedure btnBiInt64ToStringClick(Sender: TObject);
    procedure btnIBPAddClick(Sender: TObject);
    procedure btnIBPSubClick(Sender: TObject);
    procedure btnIBPMulClick(Sender: TObject);
    procedure btnIBP1RandomClick(Sender: TObject);
    procedure btnBiInt64SetStringClick(Sender: TObject);
    procedure btnIBP2RandClick(Sender: TObject);
    procedure btnIBPPowerClick(Sender: TObject);
    procedure btnIBPEvalYClick(Sender: TObject);
    procedure btnIBPEvalXClick(Sender: TObject);
    procedure btnIBPTransposeClick(Sender: TObject);
    procedure btnIBPExtractXYClick(Sender: TObject);
    procedure btnIBPIsMonicXClick(Sender: TObject);
    procedure btnIPIsMonicClick(Sender: TObject);
  private
    FQ: TCnBigNumber;
    FIP1: TCnInt64Polynomial;
    FIP2: TCnInt64Polynomial;
    FIP3: TCnInt64Polynomial;
    FRP1: TCnInt64RationalPolynomial;
    FRP2: TCnInt64RationalPolynomial;
    FRP3: TCnInt64RationalPolynomial;
    FBP1: TCnBigNumberPolynomial;
    FBP2: TCnBigNumberPolynomial;
    FBP3: TCnBigNumberPolynomial;
    FBRP1: TCnBigNumberRationalPolynomial;
    FBRP2: TCnBigNumberRationalPolynomial;
    FBRP3: TCnBigNumberRationalPolynomial;
    FIBP1: TCnInt64BiPolynomial;
    FIBP2: TCnInt64BiPolynomial;
    FIBP3: TCnInt64BiPolynomial;
  public
    { Public declarations }
  end;

var
  FormPolynomial: TFormPolynomial;

implementation

{$R *.DFM}

const
  A_BIG_POLY: string =
    '1854980275X^259+1804040276X^258+1462613043X^257+3457266643X^256+1575598888X^255' +
    '+382440497X^254+3189474550X^253+2309276060X^252+1011814753X^251+2412486366X^250+' +
    '948753555X^249+904463198X^248+134498552X^247+1539933480X^246+2828433889X^245+116' +
    '2865590X^244+2181202556X^243+3001340020X^242+1537307594X^241+2060126994X^240+123' +
    '1489752X^239+1944900859X^238+696421998X^237+1255820737X^236+1629888331X^235+1203' +
    '733232X^234+316491370X^233+2934116959X^232+3625357982X^231+1467650015X^230+22939' +
    '05206X^229+4185144083X^228+282130004X^227+1155381699X^226+1020622724X^225+575470' +
    '748X^224+1161373125X^223+1409882872X^222+1833853127X^221+1150758153X^220+5248223' +
    '80X^219+1415107493X^218+637598006X^217+3261493444X^216+4029995374X^215+248477438' +
    '4X^214+835573651X^213+1352683597X^212+3505089703X^211+2430234791X^210+368353125X' +
    '^209+1207970047X^208+884166639X^207+3643427102X^206+1876728989X^205+3779906741X^' +
    '204+4178442515X^203+1483548730X^202+3745534115X^201+3031734591X^200+3672820938X^' +
    '199+1053034031X^198+3691165039X^197+1235406183X^196+1852051556X^195+3530633181X^' +
    '194+529096580X^193+1695866063X^192+2146429873X^191+2503871643X^190+1141687519X^1' +
    '89+655983908X^188+159067912X^187+1718614925X^186+2832820880X^185+4253109410X^184' +
    '+1603049630X^183+3780265071X^182+1102145957X^181+4120462873X^180+564134145X^179+' +
    '2094933487X^178+3067154261X^177+1964305318X^176+28188425X^175+2026351893X^174+25' +
    '68908437X^173+2539308252X^172+3310527125X^171+1486765602X^170+2782237724X^169+34' +
    '12344335X^168+2381060706X^167+2653365061X^166+608636485X^165+1311813233X^164+831' +
    '757642X^163+468848950X^162+4101601325X^161+1168993923X^160+1054659035X^159+13950' +
    '78488X^158+66368800X^157+1889046539X^156+1334040486X^155+2446094055X^154+3551124' +
    '344X^153+3916233991X^152+2883111996X^151+3896099813X^150+3125617710X^149+2404891' +
    '522X^148+3007396732X^147+4149335753X^146+1999389196X^145+1038380067X^144+3314148' +
    '988X^143+4111094701X^142+2601302309X^141+3470951230X^140+4071416258X^139+8719187' +
    '89X^138+3108445476X^137+1953255921X^136+2403818563X^135+662645276X^134+392694291' +
    '5X^133+1121552056X^132+4085728401X^131+1665928879X^130+3481645192X^129+980531431' +
    'X^128+2938145989X^127+4034285931X^126+1379062218X^125+1532760386X^124+4170441799' +
    'X^123+2343586410X^122+1195015217X^121+752018821X^120+3601172408X^119+1815064397X' +
    '^118+495855536X^117+2452481235X^116+2223884265X^115+1701111663X^114+3843855743X^' +
    '113+358709178X^112+1362038216X^111+2862462533X^110+4142299799X^109+746526162X^10' +
    '8+383496405X^107+836783030X^106+787631941X^105+803385063X^104+1891213478X^103+20' +
    '6381956X^102+3314276414X^101+2362381279X^100+1740900730X^99+490620267X^98+141243' +
    '6778X^97+3295510869X^96+3569960560X^95+204719708X^94+2507402221X^93+1096677103X^' +
    '92+3618800962X^91+848798637X^90+3471755456X^89+1257317443X^88+2897207699X^87+395' +
    '636457X^86+1435658482X^85+1548446360X^84+1453241451X^83+3332036091X^82+64140068X' +
    '^81+4190723140X^80+1993693658X^79+351096328X^78+1763913494X^77+2124586699X^76+55' +
    '2176198X^75+421969190X^74+1409150395X^73+2512991512X^72+3371397996X^71+219871639' +
    '7X^70+3931751555X^69+870472535X^68+2710920941X^67+1835404420X^66+2896933695X^65+' +
    '292929179X^64+2295226146X^63+1526022735X^62+2904337903X^61+1156546232X^60+227151' +
    '9464X^59+1485046978X^58+1613396149X^57+234914362X^56+3961257650X^55+3819541314X^' +
    '54+919118649X^53+2440266491X^52+2014521249X^51+3897636155X^50+1990352818X^49+124' +
    '72192X^48+1492171128X^47+323597450X^46+1500637105X^45+4161451300X^44+1178154211X' +
    '^43+459693659X^42+2547751958X^41+1229885366X^40+744366132X^39+2586778989X^38+310' +
    '5509003X^37+4221655178X^36+598681759X^35+3631569788X^34+145944239X^33+3242043498' +
    'X^32+2721677467X^31+4190498000X^30+1705820906X^29+3078125176X^28+153198299X^27+2' +
    '67820397X^26+3510069542X^25+807224835X^24+3848578956X^23+2849635719X^22+31958842' +
    '53X^21+1854997457X^20+2258118918X^19+2713972693X^18+1896819860X^17+940455813X^16' +
    '+2311889726X^15+3355179332X^14+2268185025X^13+903223234X^12+3874343717X^11+93578' +
    '1874X^10+3686392085X^9+431008156X^8+3248217790X^7+259109695X^6+3355890688X^5+290' +
    '0960918X^4+937567694X^3+4272493694X^2+1498692482X+1613726522';

  B_BIG_POLY: string = 'X^15+77X^13+41X^12+1225X^11+1694X^10+4294954999X^9+4294965649X^8' +
    '+4294880781X^7+4294867174X^6+401359X^5+51751X^4+1332299X^3+310366X^2+882063X+123201';

  D1_BIG_POLY: string =
    '1994984457X^244+1186494340X^243+2975480792X^242+739392078X^241+3640972321X^240+' +
    '3853252916X^239+3862389564X^238+1528138348X^237+2123844723X^236+3720023116X^235+' +
    '829616883X^234+3231011463X^233+2582749856X^232+644797467X^231+2022003987X^230+24' +
    '92202256X^229+1694190540X^228+2576135824X^227+3725600557X^226+2665397466X^225+24' +
    '60062980X^224+677488245X^223+2293341938X^222+2612315942X^221+2454870850X^220+236' +
    '7024154X^219+2370086552X^218+2872160613X^217+3069043304X^216+1404357674X^215+205' +
    '7076374X^214+3061818700X^213+3594360901X^212+919505025X^211+1104344963X^210+3013' +
    '385288X^209+2783678955X^208+2238550951X^207+3932963903X^206+719397932X^205+20963' +
    '60633X^204+1663214259X^203+79694112X^202+1530503979X^201+1618849599X^200+2263923' +
    '502X^199+3071265405X^198+3957398803X^197+1103941004X^196+2607223175X^195+1424265' +
    '459X^194+4117708638X^193+2119717410X^192+2030551481X^191+3784322199X^190+4579165' +
    '9X^189+2905745156X^188+1961772807X^187+3089308597X^186+3083630929X^185+315986057' +
    '7X^184+3145429086X^183+1324751288X^182+3193924694X^181+1410045438X^180+145554660' +
    '6X^179+957250056X^178+1846136997X^177+3958377125X^176+1828116058X^175+2370323688' +
    'X^174+181056367X^173+306393706X^172+2120277324X^171+3803262239X^170+2855681620X^' +
    '169+3359727757X^168+3510832768X^167+4181105309X^166+3078727822X^165+727391239X^1' +
    '64+3110559030X^163+4258376714X^162+1193862476X^161+2581207767X^160+337851098X^15' +
    '9+187923453X^158+2386033689X^157+2963706032X^156+1878594947X^155+3599463845X^154' +
    '+391910131X^153+2506161950X^152+1749978283X^151+1393195632X^150+3522573199X^149+' +
    '1170933488X^148+145533502X^147+2797181265X^146+2372651084X^145+1207586575X^144+5' +
    '12729066X^143+3942925153X^142+4186348803X^141+3954855136X^140+108524474X^139+792' +
    '467115X^138+3021401823X^137+2298608952X^136+65161288X^135+3976607846X^134+223763' +
    '1808X^133+3582194032X^132+2909849956X^131+2170182592X^130+851087890X^129+3867957' +
    '394X^128+3976688373X^127+2081898413X^126+2040400652X^125+1666846305X^124+2042027' +
    '24X^123+3840278189X^122+759770048X^121+706027379X^120+729406081X^119+588385467X^' +
    '118+1033380477X^117+59507513X^116+2893926565X^115+3424420339X^114+3041596382X^11' +
    '3+452312161X^112+3357941608X^111+1874322716X^110+3463262426X^109+1657222714X^108' +
    '+1458013034X^107+3157218095X^106+252673704X^105+43646720X^104+3055022220X^103+32' +
    '69247108X^102+4080590702X^101+671537811X^100+468448311X^99+3914251184X^98+215217' +
    '8946X^97+876065095X^96+2946038014X^95+524187883X^94+1165325566X^93+2720883069X^9' +
    '2+595485802X^91+441740936X^90+1643500151X^89+3185103174X^88+3229531431X^87+10317' +
    '16363X^86+3918094771X^85+3895231546X^84+2019753508X^83+1960586395X^82+1586139855' +
    'X^81+736736556X^80+84454195X^79+3342636327X^78+737161488X^77+553363438X^76+34573' +
    '61173X^75+1219501058X^74+2109314794X^73+2146016784X^72+2115683775X^71+1965632924' +
    'X^70+2286950744X^69+1480833189X^68+1457946830X^67+2352132393X^66+959885573X^65+3' +
    '295777350X^64+3307919492X^63+3448822218X^62+2706024551X^61+1445827020X^60+373046' +
    '3563X^59+1257538248X^58+3852348774X^57+3410197284X^56+2546045284X^55+2209672792X' +
    '^54+4144153172X^53+1915830619X^52+3361623210X^51+2268318228X^50+253167840X^49+88' +
    '6697634X^48+231108407X^47+2411450004X^46+3231638938X^45+4026995326X^44+893380205' +
    'X^43+30794943X^42+2932700997X^41+1380776438X^40+1536534358X^39+1261681448X^38+28' +
    '07641836X^37+3476900597X^36+2157428003X^35+1950386729X^34+4218113422X^33+2140641' +
    '248X^32+854999258X^31+1988149785X^30+426454754X^29+2881224602X^28+3275336547X^27' +
    '+2860871865X^26+2457241623X^25+1066732602X^24+2978814191X^23+1397635221X^22+2729' +
    '053834X^21+2381974760X^20+1939385588X^19+3667787652X^18+1327794664X^17+395807201' +
    '1X^16+96655927X^15+2809812447X^14+3018688761X^13+2114715927X^12+4188818358X^11+1' +
    '775149856X^10+1921399828X^9+3111401727X^8+3383707369X^7+3018949045X^6+2739765062' +
    'X^5+2047288313X^4+1720897088X^3+604720261X^2+1927146615X+871506928';

  D2_BIG_POLY: string =
    '16X^15+1232X^13+656X^12+19600X^11+27104X^10+4294771019X^9+4294941419X^8+4293583' +
    '531X^7+4293365819X^6+6421744X^5+828016X^4+21316784X^3+4965856X^2+14113008X+1971216';

procedure TFormPolynomial.FormCreate(Sender: TObject);
begin
  FQ := TCnBigNumber.Create;

  FIP1 := TCnInt64Polynomial.Create;
  FIP2 := TCnInt64Polynomial.Create;
  FIP3 := TCnInt64Polynomial.Create;

  FRP1 := TCnInt64RationalPolynomial.Create;
  FRP2 := TCnInt64RationalPolynomial.Create;
  FRP3 := TCnInt64RationalPolynomial.Create;

  FBP1 := TCnBigNumberPolynomial.Create;
  FBP2 := TCnBigNumberPolynomial.Create;
  FBP3 := TCnBigNumberPolynomial.Create;

  FBRP1 := TCnBigNumberRationalPolynomial.Create;
  FBRP2 := TCnBigNumberRationalPolynomial.Create;
  FBRP3 := TCnBigNumberRationalPolynomial.Create;

  FIBP1 := TCnInt64BiPolynomial.Create;
  FIBP2 := TCnInt64BiPolynomial.Create;
  FIBP3 := TCnInt64BiPolynomial.Create;
end;

procedure TFormPolynomial.FormDestroy(Sender: TObject);
begin
  FIBP1.Free;
  FIBP2.Free;
  FIBP3.Free;

  FBRP1.Free;
  FBRP2.Free;
  FBRP3.Free;

  FBP1.Free;
  FBP2.Free;
  FBP3.Free;

  FRP1.Free;
  FRP2.Free;
  FRP3.Free;

  FIP1.Free;
  FIP2.Free;
  FIP3.Free;

  FQ.Free;
end;

procedure TFormPolynomial.btnIPCreateClick(Sender: TObject);
var
  IP: TCnInt64Polynomial;
begin
  IP := TCnInt64Polynomial.Create([23, 4, -45, 0, -78, 23, 34, 1, 0, -34, 4, -1]);
  edtIP1.Text := IP.ToString;
  IP.Free;
end;

procedure TFormPolynomial.btnIP1RandomClick(Sender: TObject);
var
  I, D: Integer;
begin
  D := StrToIntDef(edtIPDeg1.Text, 10);
  FIP1.Clear;
  Randomize;
  for I := 0 to D do
    FIP1.Add(Random(256) - 128);
  mmoIP1.Lines.Text := FIP1.ToString;
end;

procedure TFormPolynomial.btnIP2RandomClick(Sender: TObject);
var
  I, D: Integer;
begin
  D := StrToIntDef(edtIPDeg2.Text, 10);
  FIP2.Clear;
  Randomize;
  for I := 0 to D do
    FIP2.Add(Random(256) - 128);
  mmoIP2.Lines.Text := FIP2.ToString;
end;

procedure TFormPolynomial.btnIPAddClick(Sender: TObject);
begin
  if Int64PolynomialAdd(FIP3, FIP1, FIP2) then
    edtIP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnIPSubClick(Sender: TObject);
begin
  if Int64PolynomialSub(FIP3, FIP1, FIP2) then
    edtIP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnIPMulClick(Sender: TObject);
begin
  if Int64PolynomialMul(FIP3, FIP1, FIP2) then
    edtIP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnIPDivClick(Sender: TObject);
var
  R: TCnInt64Polynomial;
begin
  R := TCnInt64Polynomial.Create;

  // 测试代码
//  FIP1.SetCoefficents([1, 2, 3]);
//  FIP2.SetCoefficents([2, 1]);
//  if Int64PolynomialDiv(FIP3, R, FIP1, FIP2) then
//  begin
//    edtIP3.Text := FIP3.ToString;          // 3X - 4
//    ShowMessage('Remain: ' + R.ToString);  // 9
//  end;
  // 测试代码

  if FIP2[FIP2.MaxDegree] <> 1 then
  begin
    ShowMessage('Divisor MaxDegree only Support 1, change to 1');
    FIP2[FIP2.MaxDegree] := 1;
    mmoIP2.Lines.Text := FIP2.ToString;
  end;

  if Int64PolynomialDiv(FIP3, R, FIP1, FIP2) then
  begin
    edtIP3.Text := FIP3.ToString;
    ShowMessage('Remain: ' + R.ToString);
  end;

  // 验算 FIP3 * FIP2 + R
  Int64PolynomialMul(FIP3, FIP3, FIP2);
  Int64PolynomialAdd(FIP3, FIP3, R);
  ShowMessage(FIP3.ToString);
  if mmoIP1.Lines.Text = FIP3.ToString then
    ShowMessage('Equal Verified OK.');
  R.Free;
end;

procedure TFormPolynomial.btnTestExample1Click(Sender: TObject);
var
  X, Y, P: TCnInt64Polynomial;
begin
{
  用例一：
  构造一个有限域的二阶扩域 67*67，并指定其本原多项式是 u^2 + 1 = 0，
  然后在上面构造一条椭圆曲线 y^2 = x^3 + 4x + 3，选一个点 2u + 16, 30u + 39
  验证这个点在该椭圆曲线上。（注意 n 阶扩域上的椭圆曲线上的点的坐标是一对 n 次多项式）

  该俩用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.5

  具体实现就是计算(Y^2 - X^3 - A*X - B) mod Primtive，然后每个系数运算时都要 mod p
  这里 A = 4，B = 3。
  二阶扩域上，p 是素数 67，本原多项式是 u^2 + 1
}

  X := TCnInt64Polynomial.Create([16, 2]);
  Y := TCnInt64Polynomial.Create([39, 30]);
  P := TCnInt64Polynomial.Create([1, 0, 1]);
  try
    Int64PolynomialGaloisMul(Y, Y, Y, 67, P); // Y^2 得到 62X + 18

    Int64PolynomialMulWord(X, 4);
    Int64PolynomialSub(Y, Y, X);
    Int64PolynomialSubWord(Y, 3);             // Y 减去了 A*X - B，得到 54X + 18
    Int64PolynomialNonNegativeModWord(Y, 67);

    X.SetCoefficents([16, 2]);
    Int64PolynomialGaloisPower(X, X, 3, 67, P);  // 得到 54X + 18

    Int64PolynomialGaloisSub(Y, Y, X, 67);
    Int64PolynomialGaloisMod(Y, Y, P, 67);    // 算出 0
    ShowMessage(Y.ToString);
  finally
    P.Free;
    Y.Free;
    X.Free;
  end;
end;

procedure TFormPolynomial.btnTestExample2Click(Sender: TObject);
var
  X, Y, P: TCnInt64Polynomial;
begin
{
  用例二：
  构造一个有限域的二阶扩域 7691*7691，并指定其本原多项式是 u^2 + 1 = 0，
  然后在上面构造一条椭圆曲线 y^2=x^3+1 mod 7691，选一个点 633u + 6145, 7372u + 109
  验证这个点在该椭圆曲线上。

  该俩用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 4.0.1

  具体实现就是计算(Y^2 - X^3 - A*X - B) mod Primtive，然后每个系数运算时都要 mod p
  这里 A = 0，B = 1
  二阶扩域上，p 是素数 7691，本原多项式是 u^2 + 1
}

  X := TCnInt64Polynomial.Create([6145, 633]);
  Y := TCnInt64Polynomial.Create([109, 7372]);
  P := TCnInt64Polynomial.Create([1, 0, 1]);
  try
    Int64PolynomialGaloisMul(Y, Y, Y, 7691, P);

    Int64PolynomialSubWord(Y, 1);
    Int64PolynomialNonNegativeModWord(Y, 7691);

    X.SetCoefficents([6145, 633]);
    Int64PolynomialGaloisPower(X, X, 3, 7691, P);

    Int64PolynomialGaloisSub(Y, Y, X, 7691);
    Int64PolynomialGaloisMod(Y, Y, P, 7691);    // 算出 0
    ShowMessage(Y.ToString);
  finally
    P.Free;
    Y.Free;
    X.Free;
  end;
end;

procedure TFormPolynomial.btnTestExample3Click(Sender: TObject);
var
  X, P: TCnInt64Polynomial;
begin
{
  用例三：
  构造一个有限域的二阶扩域 67*67，并指定其本原多项式是 u^2 + 1 = 0，
  验证：(2u + 16)^67 = 65u + 16, (30u + 39)^67 = 37u + 39

  该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.5
}

  X := TCnInt64Polynomial.Create([16, 2]);
  P := TCnInt64Polynomial.Create([1, 0, 1]);
  try
    Int64PolynomialGaloisPower(X, X, 67, 67, P);
    ShowMessage(X.ToString);   // 得到 65x + 16

    X.SetCoefficents([39, 30]);
    Int64PolynomialGaloisPower(X, X, 67, 67, P);
    ShowMessage(X.ToString);   // 得到 37x + 39
  finally
    X.Free;
    P.Free;
  end;
end;

procedure TFormPolynomial.btnTestExample4Click(Sender: TObject);
var
  X, P: TCnInt64Polynomial;
begin
{
  用例四：
  构造一个有限域的三阶扩域 67*67*67，并指定其本原多项式是 u^3 + 2 = 0，
  验证：
  (15v^2 + 4v + 8)^67  = 33v^2 + 14v + 8, 44v^2 + 30v + 21)^67 = 3v^2 + 38v + 21
  (15v^2 + 4v + 8)^(67^2)  = 19v^2 + 49v + 8, (44v^2 + 30v + 21)^(67^2) = 20v^2 + 66v + 21
  (15v^2 + 4v + 8)^(67^3)  = 15v^2 + 4v + 8,  (44v^2 + 30v + 21)^(67^3) = 44v^2 + 30v + 21 都回到自身

  该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.5
}

  X := TCnInt64Polynomial.Create;
  P := TCnInt64Polynomial.Create([2, 0, 0, 1]);
  try
    X.SetCoefficents([8, 4, 15]);
    Int64PolynomialGaloisPower(X, X, 67, 67, P);
    ShowMessage(X.ToString);  // 33x^2 + 14x + 8

    X.SetCoefficents([21, 30, 44]);
    Int64PolynomialGaloisPower(X, X, 67, 67, P);
    ShowMessage(X.ToString);  // 3x^2 + 38x + 21

    X.SetCoefficents([8, 4, 15]);
    Int64PolynomialGaloisPower(X, X, 67 * 67, 67, P);
    ShowMessage(X.ToString);  // 19x^2 + 49x + 8

    X.SetCoefficents([21, 30, 44]);
    Int64PolynomialGaloisPower(X, X, 67 * 67, 67, P);
    ShowMessage(X.ToString);  // 20x^2 + 66x + 21

    X.SetCoefficents([8, 4, 15]);
    Int64PolynomialGaloisPower(X, X, 67 * 67 * 67, 67, P);
    ShowMessage(X.ToString);  // 15x^2 + 4x + 8

    X.SetCoefficents([21, 30, 44]);
    Int64PolynomialGaloisPower(X, X, 67 * 67 * 67, 67, P);
    ShowMessage(X.ToString);  // 44x^2 + 30x + 21
  finally
    X.Free;
    P.Free;
  end;
end;

procedure TFormPolynomial.btnGaloisOnCurveClick(Sender: TObject);
var
  Ecc: TCnInt64PolynomialEcc;
begin
{
  用例一：
  椭圆曲线 y^2 = x^3 + 4x + 3, 如果定义在二次扩域 F67^2 上，本原多项式 u^2 + 1
  判断基点 P(2u+16, 30u+39) 在曲线上

  用例二：
  椭圆曲线 y^2 = x^3 + 4x + 3, 如果定义在三次扩域 F67^3 上，本原多项式 u^3 + 2
  判断基点 P((15v^2 + 4v + 8, 44v^2 + 30v + 21)) 在曲线上

  该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.8
}

  Ecc := TCnInt64PolynomialEcc.Create(4, 3, 67, 2, [16, 2], [39, 30], 0, [1, 0,
    1]); // Order 未指定，先不传
  if Ecc.IsPointOnCurve(Ecc.Generator) then
    ShowMessage('Ecc 1 Generator is on Curve')
  else
    ShowMessage('Error');

  Ecc.Free;

  Ecc := TCnInt64PolynomialEcc.Create(4, 3, 67, 3, [8, 4, 15], [21, 30, 44], 0,
    [2, 0, 0, 1]); // Order 未指定，先不传
  if Ecc.IsPointOnCurve(Ecc.Generator) then
    ShowMessage('Ecc 2 Generator is on Curve')
  else
    ShowMessage('Error');

  Ecc.Free;
end;

procedure TFormPolynomial.btnPolyGcdClick(Sender: TObject);
begin
  if (FIP1[FIP1.MaxDegree] <> 1) and (FIP2[FIP2.MaxDegree] <> 1) then
  begin
    ShowMessage('Divisor MaxDegree only Support 1, change to 1');
    FIP1[FIP1.MaxDegree] := 1;
    mmoIP1.Lines.Text := FIP1.ToString;
    FIP2[FIP2.MaxDegree] := 1;
    mmoIP2.Lines.Text := FIP2.ToString;
  end;

//  FIP1.SetCoefficents([-5, 2, 0, 3]);
//  FIP2.SetCoefficents([-1, -2, 0, 3]);
  if Int64PolynomialGreatestCommonDivisor(FIP3, FIP1, FIP2) then
    edtIP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnGaloisTestGcdClick(Sender: TObject);
begin
// GCD 例子一：
// F11 扩域上的 x^2 + 8x + 7 和 x^3 + 7x^2 + x + 7 的最大公因式是 x + 7
  FIP1.SetCoefficents([7, 8, 1]);
  FIP2.SetCoefficents([7, 1, 7, 1]);  // 而和 [7, 1, 2, 1] 则互素
  if Int64PolynomialGaloisGreatestCommonDivisor(FIP3, FIP1, FIP2, 11) then
    ShowMessage(FIP3.ToString);

// GCD 例子二：
// F2 扩域上的 x^6 + x^5 + x^4 + x^3 + x^2 + x + 1 和 x^4 + x^2 + x + 1 的最大公因式是 x^3 + x^2 + 1
  FIP1.SetCoefficents([1,1,1,1,1,1,1]);
  FIP2.SetCoefficents([1,1,1,0,1]);
  if Int64PolynomialGaloisGreatestCommonDivisor(FIP3, FIP1, FIP2, 2) then
    ShowMessage(FIP3.ToString);
end;

procedure TFormPolynomial.btnTestGaloisMIClick(Sender: TObject);
begin
// Modulus Inverse 例子：
// F3 的扩域上的本原多项式 x^3 + 2x + 1 有 x^2 + 1 的模逆多项式为 2x^2 + x + 2
  FIP1.SetCoefficents([1, 0, 1]);
  FIP2.SetCoefficents([1, 2, 0, 1]);
  Int64PolynomialGaloisModularInverse(FIP3, FIP1, FIP2, 3);
    edtIP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnGF28Test1Click(Sender: TObject);
var
  IP: TCnInt64Polynomial;
begin
  FIP1.SetCoefficents([1,1,1,0,1,0,1]); // 57
  FIP2.SetCoefficents([1,1,0,0,0,0,0,1]); // 83
  FIP3.SetCoefficents([1,1,0,1,1,0,0,0,1]); // 本原多项式

  IP := TCnInt64Polynomial.Create;
  Int64PolynomialGaloisMul(IP, FIP1, FIP2, 2, FIP3);
  edtIP3.Text := IP.ToString;  // 得到 1,0,0,0,0,0,1,1
  IP.Free;
end;

procedure TFormPolynomial.btnEccPointAddClick(Sender: TObject);
var
  Ecc: TCnInt64PolynomialEcc;
  P, Q, S: TCnInt64PolynomialEccPoint;
begin
// 有限扩域上的多项式椭圆曲线点加
// F67^2 上的椭圆曲线 y^2 = x^3 + 4x + 3 本原多项式 u^2 + 1
// 点 P(2u + 16, 30u + 39) 满足 68P + 11πP = 0
// 其中 πP 是 P 的 Frob 映射也就是 X Y 各 67 次方为用例三中的(65u + 16, 37u + 39)

// 该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.8

  Ecc := TCnInt64PolynomialEcc.Create(4, 3, 67, 2, [16, 2], [39, 30], 0, [1, 0,
    1]); // Order 未指定，先不传

  P := TCnInt64PolynomialEccPoint.Create;
  P.Assign(Ecc.Generator);
  Ecc.MultiplePoint(68, P);
  ShowMessage(P.ToString);  // 37x+31,51x+39       // 15x+6, 63x+4

  Q := TCnInt64PolynomialEccPoint.Create;
  Q.Assign(Ecc.Generator);
  Int64PolynomialGaloisPower(Q.X, Q.X, 67, 67, Ecc.Primitive);
  Int64PolynomialGaloisPower(Q.Y, Q.Y, 67, 67, Ecc.Primitive);

  Ecc.MultiplePoint(11, Q);
  ShowMessage(Q.ToString);   // 18x+30,41x+49      // 39x+2, 38x+48

  S := TCnInt64PolynomialEccPoint.Create;
  Ecc.PointAddPoint(S, P, Q);
  ShowMessage(S.ToString);   // 0, 0

  S.Free;
  P.Free;
  Q.Free;
  Ecc.Free;
end;

procedure TFormPolynomial.btnTestEccPointAdd2Click(Sender: TObject);
var
  Ecc: TCnInt64PolynomialEcc;
  P, Q, S: TCnInt64PolynomialEccPoint;
begin
// 有限扩域上的多项式椭圆曲线点加
// F67^3 上的椭圆曲线 y^2 = x^3 + 4x + 3 本原多项式 u^3 + 2
// 点 P(15v^2 + 4v + 8, 44v^2 + 30v + 21) 满足 π2P - (-11)πP + 67P = 0
// 其中 πP 是 P 的 Frob 映射也就是 X Y 各 67 次方
// πP为用例四中的(33v^2 + 14v + 8, 3v^2 + 38v + 21)
// π2P为用例四中的 67^2 次方(19v^2 + 49v + 8, 20v^2 + 66v + 21)

// 该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.8

  Ecc := TCnInt64PolynomialEcc.Create(4, 3, 67, 3, [8, 4, 15], [21, 30, 44], 0, [2,
    0, 0 ,1]); // Order 未指定，先不传

  P := TCnInt64PolynomialEccPoint.Create;
  P.Assign(Ecc.Generator);
  Ecc.MultiplePoint(67, P);                                        // 算 67P

  Q := TCnInt64PolynomialEccPoint.Create;
  Q.Assign(Ecc.Generator);
  Int64PolynomialGaloisPower(Q.X, Q.X, 67, 67, Ecc.Primitive);
  Int64PolynomialGaloisPower(Q.Y, Q.Y, 67, 67, Ecc.Primitive);   // 算 πP
  Ecc.MultiplePoint(-11, Q);                                       // 算 -11πp

  S := TCnInt64PolynomialEccPoint.Create;
  Ecc.PointSubPoint(S, P, Q);

  Q.Assign(Ecc.Generator);
  Int64PolynomialGaloisPower(Q.X, Q.X, 67*67, 67, Ecc.Primitive);
  Int64PolynomialGaloisPower(Q.Y, Q.Y, 67*67, 67, Ecc.Primitive); // 算 π2P

  Ecc.PointAddPoint(S, S, Q);
  ShowMessage(Q.ToString);                                          // 得到 0,0

  P.Free;
  Q.Free;
  S.Free;
  Ecc.Free;
end;

procedure TFormPolynomial.btnTestDivPolyClick(Sender: TObject);
var
  P: TCnInt64Polynomial;
begin
  // 验证可除多项式的生成
  // 如在 F101 上定义的椭圆曲线: y^2 = x^3 + x + 1
  // 用例数据不完整只能认为基本通过
  // 该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.9

  P := TCnInt64Polynomial.Create;

  Int64PolynomialGaloisCalcDivisionPolynomial(1, 1, 0, P, 101);
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(1, 1, 1, P, 101);
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(1, 1, 2, P, 101);
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(1, 1, 3, P, 101);  // 3x4 +6x2+12x+100
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(1, 1, 4, P, 101);  // ...
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(1, 1, 5, P, 101);  // 5x12 ... 16
  ShowMessage(P.ToString);

  P.Free;
end;

procedure TFormPolynomial.btnTestDivPoly2Click(Sender: TObject);
var
  P: TCnInt64Polynomial;
begin
  // 验证可除多项式的生成
  // 如在 F13 上定义的椭圆曲线: y^2 = x^3 + 2x + 1
  // 用例数据不完整只能认为基本通过
  // 该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.10

  P := TCnInt64Polynomial.Create;

  Int64PolynomialGaloisCalcDivisionPolynomial(2, 1, 0, P, 13);
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(2, 1, 1, P, 13);
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(2, 1, 2, P, 13);
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(2, 1, 3, P, 13);  // 3x4 +12x2+12x+9
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(2, 1, 4, P, 13);  // ...
  ShowMessage(P.ToString);
  Int64PolynomialGaloisCalcDivisionPolynomial(2, 1, 5, P, 13);  // 5x12 ... 6x + 7
  ShowMessage(P.ToString);

  P.Free;
end;

procedure TFormPolynomial.btnTestGaloisPoint2Click(Sender: TObject);
var
  X, Y, P, E: TCnInt64Polynomial;
begin
  X := TCnInt64Polynomial.Create([12, 8, 11, 1]);
  Y := TCnInt64Polynomial.Create([12, 5, 2, 12]);
  P := TCnInt64Polynomial.Create([9, 12, 12, 0, 3]);
  E := TCnInt64Polynomial.Create([1, 2, 0, 1]);

  Int64PolynomialGaloisMul(Y, Y, Y, 13, P); // 计算 PiY 系数的平方
  Int64PolynomialGaloisMul(Y, Y, E, 13, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点 2x3+7x2+12x+5

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入，也得到 2x3+7x2+12x+5
  Int64PolynomialGaloisCompose(E, E, X, 13, P);

  if Int64PolynomialEqual(E, Y) then
    ShowMessage('Pi On Curve')
  else
    ShowMessage('Pi NOT');

  E.Free;
  P.Free;
  Y.Free;
  X.Free;
end;

procedure TFormPolynomial.btnTestPolyPoint2Click(Sender: TObject);
var
  X, Y, P, E: TCnInt64Polynomial;
begin
  X := TCnInt64Polynomial.Create([12,11,5,6]);
  Y := TCnInt64Polynomial.Create([8,5]);
  P := TCnInt64Polynomial.Create([9, 12, 12, 0, 3]);
  E := TCnInt64Polynomial.Create([1, 2, 0, 1]);

  Int64PolynomialGaloisMul(Y, Y, Y, 13, P); // 计算 PiY 系数的平方
  Int64PolynomialGaloisMul(Y, Y, E, 13, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点 2x3+7x2+12x+5

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入，也得到 2x3+7x2+12x+5
  Int64PolynomialGaloisCompose(E, E, X, 13, P);

  if Int64PolynomialEqual(E, Y) then
    ShowMessage('Pi^2 On Curve')
  else
    ShowMessage('Pi^2 NOT');

  E.Free;
  P.Free;
  Y.Free;
  X.Free;
end;

procedure TFormPolynomial.btnTestPolyEccPoint3Click(Sender: TObject);
var
  X, Y, P, E: TCnInt64Polynomial;
begin
  E := TCnInt64Polynomial.Create([1, 2, 0, 1]);
  P := TCnInt64Polynomial.Create([7, 6, 1, 9, 10, 11, 12, 12, 9, 3, 7, 5]);

  // 某 Pi
  X := TCnInt64Polynomial.Create([9,4,5,6,11,3,8,8,6,2,9]);
  Y := TCnInt64Polynomial.Create([12,1,11,0,1,1,7,1,8,9,12,7]);

  Int64PolynomialGaloisMul(Y, Y, Y, 13, P); // 计算 PiY 系数的平方
  Int64PolynomialGaloisMul(Y, Y, E, 13, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入
  Int64PolynomialGaloisCompose(E, E, X, 13, P);

  if Int64PolynomialEqual(E, Y) then
    ShowMessage('Pi On Curve')
  else
    ShowMessage('Pi NOT');

  // 某 Pi^2
  X.SetCoefficents([5,11,3,2,2,7,5,2,11,6,12,5]);
  Y.SetCoefficents([9,3,9,9,2,10,5,3,5,6,2,6]);

  Int64PolynomialGaloisMul(Y, Y, Y, 13, P); // 计算 PiY 系数的平方
  Int64PolynomialGaloisMul(Y, Y, E, 13, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入
  Int64PolynomialGaloisCompose(E, E, X, 13, P);

  if Int64PolynomialEqual(E, Y) then
    ShowMessage('Pi^2 On Curve')
  else
    ShowMessage('Pi^2 NOT');

  // 某 3 * P
  X.SetCoefficents([10,8,7,9,5,12,4,12,3,4,1,6]);
  Y.SetCoefficents([7,2,10,0,3,7,4,6,3,0,11,12]);

  Int64PolynomialGaloisMul(Y, Y, Y, 13, P); // 计算 PiY 系数的平方
  Int64PolynomialGaloisMul(Y, Y, E, 13, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入
  Int64PolynomialGaloisCompose(E, E, X, 13, P);

  if Int64PolynomialEqual(E, Y) then
    ShowMessage('3 * P On Curve')
  else
    ShowMessage('3 * P NOT');

  // 某点加 π^2 + 3 * P
  X.SetCoefficents([4,5,1,11,4,4,9,6,12,2,6,3]);
  Y.SetCoefficents([2,7,9,11,7,2,9,5,5,6,12,3]);

  Int64PolynomialGaloisMul(Y, Y, Y, 13, P); // 计算 PiY 系数的平方
  Int64PolynomialGaloisMul(Y, Y, E, 13, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入
  Int64PolynomialGaloisCompose(E, E, X, 13, P);

  if Int64PolynomialEqual(E, Y) then
    ShowMessage('Pi^2 + 3 * P On Curve')
  else
    ShowMessage('Pi^2 + 3 * P NOT');

  E.Free;
  P.Free;
  Y.Free;
  X.Free;
end;

procedure TFormPolynomial.btnTestGaloisPolyMulModClick(Sender: TObject);
var
  P, Q, H: TCnInt64Polynomial;
begin
  P := TCnInt64Polynomial.Create([11,0,6,12]);
  Q := TCnInt64Polynomial.Create([2,4,0,2]);
  H := TCnInt64Polynomial.Create([9,12,12,0,3]);
  Int64PolynomialGaloisMul(P, P, Q, 13, H);
  ShowMessage(P.ToString); // 4x3+6x2+5x+10

  H.Free;
  Q.Free;
  P.Free;
end;

procedure TFormPolynomial.btnTestGaloisModularInverse1Click(
  Sender: TObject);
begin
  FIP1.SetCoefficents([1,2,0,1]);
  FIP2.SetCoefficents([3,4,4,0,1]);  // 本原多项式
  Int64PolynomialGaloisModularInverse(FIP3, FIP1, FIP2, 13);
  ShowMessage(FIP3.ToString);  // 得到 5x^3+6x+2

  Int64PolynomialGaloisMul(FIP3, FIP3, FIP1, 13, FIP2); // 乘一下验算看是不是得到 1
  ShowMessage(FIP3.ToString);

  FIP1.SetCoefficents([4,8,0,4]);
  FIP2.SetCoefficents([9,12,12,0,3]);
  Int64PolynomialGaloisModularInverse(FIP3, FIP1, FIP2, 13);
  ShowMessage(FIP3.ToString);  // 得到 11x^3+8x+7

  // 以下不用，
//  FIP1.SetCoefficents([4,-8,-4,0,1]);
//  Int64PolynomialGaloisMul(FIP1, FIP1, FIP3, 13, FIP2);
//  // Int64PolynomialGaloisMul(FIP3, FIP3, FIP1, 13, FIP2); // 乘一下验算看是不是得到 1
//  ShowMessage(FIP1.ToString); // 居然得到 x
end;

procedure TFormPolynomial.btnTestEuclid2Click(Sender: TObject);
var
  A, B, X, Y: TCnInt64Polynomial;
begin
  A := TCnInt64Polynomial.Create([0,6]);
  B := TCnInt64Polynomial.Create([3]);
  X := TCnInt64Polynomial.Create;
  Y := TCnInt64Polynomial.Create;

  // 求 6x * X + 3 * Y = 1 mod 13 的解，得到 0，9
  Int64PolynomialGaloisExtendedEuclideanGcd(A, B, X, Y, 13);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  A.Free;
  B.Free;
  X.Free;
  Y.Free;
end;

procedure TFormPolynomial.btnTestExtendEuclid3Click(Sender: TObject);
var
  A, B, X, Y: TCnInt64Polynomial;
begin
  A := TCnInt64Polynomial.Create([3,3,2]);
  B := TCnInt64Polynomial.Create([0,6]);
  X := TCnInt64Polynomial.Create;
  Y := TCnInt64Polynomial.Create;

  // 求 2x2+3x+3 * X - 6x * Y = 1 mod 13 的解，应该得到 9 和 10x+2
  Int64PolynomialGaloisExtendedEuclideanGcd(A, B, X, Y, 13);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  A.Free;
  B.Free;
  X.Free;
  Y.Free;
end;

procedure TFormPolynomial.btnTestGaloisDivClick(Sender: TObject);
var
  A, B, C, D: TCnInt64Polynomial;
begin
  // GF13 上 (2x^2+3x+3) div (6x) 应该等于 9x + 7 余 3
  A := TCnInt64Polynomial.Create([3,3,2]);
  B := TCnInt64Polynomial.Create([0,6]);
  C := TCnInt64Polynomial.Create;
  D := TCnInt64Polynomial.Create;
  Int64PolynomialGaloisDiv(C, D, A, B, 13);
  ShowMessage(C.ToString); // 9x + 7
  ShowMessage(D.ToString); // 3
  A.Free;
  B.Free;
  C.Free;
  D.Free;
end;

procedure TFormPolynomial.btnTestEccDivisionPoly3Click(Sender: TObject);
var
  DP: TCnInt64Polynomial;
  A, B, P, V, X1, X2: Int64;
begin
  // Division Polynomial 测试用例，来自 John J. McGee 的
  // 《Rene Schoof's Algorithm for Determing the Order of the Group of Points
  //    on an Elliptic Curve over a Finite Field》第 31 页
  A := 46;
  B := 74;
  P := 97;

  X1 := 4;
  X2 := 90;

  DP := TCnInt64Polynomial.Create;
  mmoTestDivisionPolynomial.Lines.Clear;

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 2, DP, P);
  mmoTestDivisionPolynomial.Lines.Add('2: === ' + DP.ToString);
  V := Int64PolynomialGaloisGetValue(DP, X1, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 2
  V := Int64PolynomialGaloisGetValue(DP, X2, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 2

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 3, DP, P);
  mmoTestDivisionPolynomial.Lines.Add('3: === ' + DP.ToString);
  V := Int64PolynomialGaloisGetValue(DP, X1, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 24
  V := Int64PolynomialGaloisGetValue(DP, X2, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 76

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 4, DP, P);
  mmoTestDivisionPolynomial.Lines.Add('4: === ' + DP.ToString);
  V := Int64PolynomialGaloisGetValue(DP, X1, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 0
  V := Int64PolynomialGaloisGetValue(DP, X2, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 14

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 5, DP, P);
  mmoTestDivisionPolynomial.Lines.Add('5: === ' + DP.ToString);
  V := Int64PolynomialGaloisGetValue(DP, X1, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 47
  V := Int64PolynomialGaloisGetValue(DP, X2, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 0

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 6, DP, P);
  mmoTestDivisionPolynomial.Lines.Add('6: === ' + DP.ToString);
  V := Int64PolynomialGaloisGetValue(DP, X1, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 25
  V := Int64PolynomialGaloisGetValue(DP, X2, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 21

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 7, DP, P);
  mmoTestDivisionPolynomial.Lines.Add('7: === ' + DP.ToString);
  V := Int64PolynomialGaloisGetValue(DP, X1, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 22
  V := Int64PolynomialGaloisGetValue(DP, X2, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 23

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 8, DP, P);
  mmoTestDivisionPolynomial.Lines.Add('8: === ' + DP.ToString);
  V := Int64PolynomialGaloisGetValue(DP, X1, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 0，
  V := Int64PolynomialGaloisGetValue(DP, X2, P);
  mmoTestDivisionPolynomial.Lines.Add(IntToStr(V));                                    // 得到 31

  DP.Free;
end;

procedure TFormPolynomial.btnGenerateDivisionPolynomialClick(
  Sender: TObject);
var
  List: TObjectList;
  I: Integer;
begin
  List := TObjectList.Create(True);
  CnInt64GenerateGaloisDivisionPolynomials(46, 74, 97, 20, List);

  mmoTestDivisionPolynomial.Lines.Clear;
  for I := 0 to List.Count - 1 do
    mmoTestDivisionPolynomial.Lines.Add(TCnInt64Polynomial(List[I]).ToString);

  List.Free;
end;

procedure TFormPolynomial.btnRP2PointClick(Sender: TObject);
var
  X, Y: TCnInt64RationalPolynomial;
begin
  X := TCnInt64RationalPolynomial.Create;
  Y := TCnInt64RationalPolynomial.Create;

  X.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.SetOne;

  TCnInt64PolynomialEcc.RationalMultiplePoint(2, X, Y, 1, 1, 23);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  if TCnInt64PolynomialEcc.IsRationalPointOnCurve(X, Y, 1, 1, 23) then
    ShowMessage('2*P On Curve')
  else
    ShowMessage('2*P NOT On Curve');

  // 验证 6 19 的二倍点是 13 16
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(X, 6, 23))); // 得到 13 对了
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(Y, 6, 23) * 19 mod 23)); // 得到 16 对了

  X.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.SetOne;

  TCnInt64PolynomialEcc.RationalMultiplePoint(3, X, Y, 1, 1, 23);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  if TCnInt64PolynomialEcc.IsRationalPointOnCurve(X, Y, 1, 1, 23) then
    ShowMessage('3*P On Curve')
  else
    ShowMessage('3*P NOT On Curve');

  // 验证 6 19 的三倍点是 7 11
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(X, 6, 23))); // 得到 7 对了
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(Y, 6, 23) * 19 mod 23)); // 得到 11 对了

  X.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.SetOne;

  TCnInt64PolynomialEcc.RationalMultiplePoint(4, X, Y, 1, 1, 23);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  if TCnInt64PolynomialEcc.IsRationalPointOnCurve(X, Y, 1, 1, 23) then
    ShowMessage('4*P On Curve')
  else
    ShowMessage('4*P NOT On Curve');

  // 验证 6 19 的四倍点是 5 19
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(X, 6, 23))); // 得到 5
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(Y, 6, 23) * 19 mod 23)); // 得到 19

  X.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.SetOne;

  TCnInt64PolynomialEcc.RationalMultiplePoint(5, X, Y, 1, 1, 23);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  if TCnInt64PolynomialEcc.IsRationalPointOnCurve(X, Y, 1, 1, 23) then
    ShowMessage('5*P On Curve')
  else
    ShowMessage('5*P NOT On Curve');

  // 验证 6 19 的五倍点是 12 4
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(X, 6, 23))); // 得到 12
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(Y, 6, 23) * 19 mod 23)); // 得到 4

  // 多项式本身也符合曲线方程

  X.Free;
  Y.Free;
end;

procedure TFormPolynomial.btnRationalPolynomialGenerateClick(
  Sender: TObject);
var
  I, D: Integer;
begin
  D := 2;
  FRP1.SetZero;
  FRP2.SetZero;

  Randomize;
  for I := 0 to D do
  begin
    FRP1.Nominator.Add(Random(16) - 1);
    FRP2.Nominator.Add(Random(16) - 1);
    FRP1.Denominator.Add(Random(16) - 1);
    FRP2.Denominator.Add(Random(16) - 1);
  end;

  edtRationalNominator1.Text := FRP1.Nominator.ToString;
  edtRationalNominator2.Text := FRP2.Nominator.ToString;
  edtRationalDenominator1.Text := FRP1.Denominator.ToString;
  edtRationalDenominator2.Text := FRP2.Denominator.ToString;
end;

procedure TFormPolynomial.btnRationalPolynomialAddClick(Sender: TObject);
begin
  if chkRationalPolynomialGalois.Checked then
    Int64RationalPolynomialGaloisAdd(FRP1, FRP2, FRP3, StrToInt(edtRationalPolynomialPrime.Text))
  else
    Int64RationalPolynomialAdd(FRP1, FRP2, FRP3);
  edtRationalResultNominator.Text := FRP3.Nominator.ToString;
  edtRationalResultDenominator.Text := FRP3.Denominator.ToString;
end;

procedure TFormPolynomial.btnRationalPolynomialSubClick(Sender: TObject);
begin
  if chkRationalPolynomialGalois.Checked then
    Int64RationalPolynomialGaloisSub(FRP1, FRP2, FRP3, StrToInt(edtRationalPolynomialPrime.Text))
  else
    Int64RationalPolynomialSub(FRP1, FRP2, FRP3);
  edtRationalResultNominator.Text := FRP3.Nominator.ToString;
  edtRationalResultDenominator.Text := FRP3.Denominator.ToString;
end;

procedure TFormPolynomial.btnRationalPolynomialMulClick(Sender: TObject);
begin
  if chkRationalPolynomialGalois.Checked then
    Int64RationalPolynomialGaloisMul(FRP1, FRP2, FRP3, StrToInt(edtRationalPolynomialPrime.Text))
  else
    Int64RationalPolynomialMul(FRP1, FRP2, FRP3);
  edtRationalResultNominator.Text := FRP3.Nominator.ToString;
  edtRationalResultDenominator.Text := FRP3.Denominator.ToString;
end;

procedure TFormPolynomial.btnRationalPolynomialDivClick(Sender: TObject);
begin
  if chkRationalPolynomialGalois.Checked then
    Int64RationalPolynomialGaloisDiv(FRP1, FRP2, FRP3, StrToInt(edtRationalPolynomialPrime.Text))
  else
    Int64RationalPolynomialDiv(FRP1, FRP2, FRP3);
  edtRationalResultNominator.Text := FRP3.Nominator.ToString;
  edtRationalResultDenominator.Text := FRP3.Denominator.ToString;
end;

procedure TFormPolynomial.btnManualOnCurveClick(Sender: TObject);
var
  A, B, Q: Int64;
  X, Y: TCnInt64RationalPolynomial;
  P, Y2: TCnInt64Polynomial;
  RL, RR, T: TCnInt64RationalPolynomial;
begin
  // 简单椭圆曲线二倍点用可除多项式手工计算的结果验证，通过
  X := TCnInt64RationalPolynomial.Create;
  Y := TCnInt64RationalPolynomial.Create;
  Y2 := TCnInt64Polynomial.Create;
  P := TCnInt64Polynomial.Create;

  RL := TCnInt64RationalPolynomial.Create;
  RR := TCnInt64RationalPolynomial.Create;
  T := TCnInt64RationalPolynomial.Create;

  A := 1;
  B := 1;
  Q := 23;   // 有限域F23上的 Y^2=X^3+X+1  （6，19）* 2 = （13，16）

  // 先求整数域
  X.Nominator.SetCoefficents([A*A, 4-12*B, 4-6*A, 0, 1]);  //  X4 + (4-6A)X2 + (4- 12B)x + A2
  X.Denominator.SetCoefficents([4*B, 4*A, 0, 4]);         //        4X3 + 4AX + 4B

  Y.Nominator.SetCoefficents([-A*A*A-8*B*B, -4*A*B, -5*A*A, 20*B, 5*A, 0, 1]); // X6 + 5AX4 + 20BX3 - 5A2X2 - 4ABX - 8B2 - A3
  Y.Denominator.SetCoefficents([8*B*B, 16*A*B, 8*A*A, 16*B, 16*A, 0, 8]);      //          8(X3+AX+B)(X3+AX+B)

  Y2.SetCoefficents([B, A, 0, 1]);
  // 验证 Y^2 * (x^3+Ax+B) 是否等于 X3 + AX + B

  Int64RationalPolynomialMul(Y, Y, Y);
  Int64RationalPolynomialMul(Y, Y2, RL); // 得到 Y^2 (x^3+Ax+B)
  RL.Reduce;
  ShowMessage(RL.ToString);

  Int64RationalPolynomialMul(X, X, RR);
  Int64RationalPolynomialMul(RR, X, RR); // 得到 X^3

  P.SetCoefficents([A]);
  Int64RationalPolynomialMul(X, P, T);   // T 得到 A * X
  Int64RationalPolynomialAdd(RR, T, RR); // RR 得到 X^3 + AX

  P.SetCoefficents([B]);
  Int64RationalPolynomialAdd(RR, P, RR); // RR 得到 X^3 + AX + B
  RR.Reduce;
  ShowMessage(RR.ToString);

  // RL/RR 在整数域内有除式不等，换 Fq 看看，原始点（6，19），二倍点公式套上去得到（13，16）
  X.Nominator.SetCoefficents([A*A, 4-12*B, 4-6*A, 0, 1]);  //  X4 + (4-6A)X2 + (4- 12B)x + A2
  X.Denominator.SetCoefficents([4*B, 4*A, 0, 4]);          //        4X3 + 4AX + 4B
  ShowMessage('2*X (X=6) using Division Polynomial is '
    + IntToStr(Int64RationalPolynomialGaloisGetValue(X, 6, Q))); // 得到 13 对了

  Y.Nominator.SetCoefficents([-A*A*A-8*B*B, -4*A*B, -5*A*A, 20*B, 5*A, 0, 1]); // X6 + 5AX4 + 20BX3 - 5A2X2 - 4ABX - 8B2 - A3
  Y.Denominator.SetCoefficents([8*B*B, 16*A*B, 8*A*A, 16*B, 16*A, 0, 8]);      //          8(X3+AX+B)(X3+AX+B)
  ShowMessage('2*Y (X=6) using Division Polynomial is '
    + IntToStr((Int64RationalPolynomialGaloisGetValue(Y, 6, Q) * 19) mod Q)); // 得到 16 对了

  Y2.SetCoefficents([B, A, 0, 1]);
  // 验证二倍点公式用一倍点坐标算出来的值 Y^2 * (x^3+Ax+B) 是否等于 X3 + AX + B

  Int64RationalPolynomialGaloisMul(Y, Y, Y, Q);
  Int64RationalPolynomialGaloisMul(Y, Y2, RL, Q); // 得到 Y^2 (x^3+Ax+B)
  ShowMessage(RL.ToString);

  Int64RationalPolynomialGaloisMul(X, X, RR, Q);
  Int64RationalPolynomialGaloisMul(RR, X, RR, Q); // 得到 X^3

  P.SetCoefficents([A]);
  Int64RationalPolynomialGaloisMul(X, P, T, Q);   // T 得到 A * X
  Int64RationalPolynomialGaloisAdd(RR, T, RR, Q); // RR 得到 X^3 + AX

  P.SetCoefficents([B]);
  Int64RationalPolynomialGaloisAdd(RR, P, RR, Q); // RR 得到 X^3 + AX + B
  ShowMessage(RR.ToString);

  // RL/RR 在 F23 内表达式还是不等，但各自求值看看，居然相等！
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(RL, 6, Q)));  // 3 = 二倍点 Y 坐标平方 16^2 mod 23 = 3
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(RR, 6, Q)));  // 3 = 二倍点 X 坐标 13^3 + 13 + 1 mod 23 = 3

  // 再拿另外一个点 （13，16）的二倍点（5，19）试一试，也对
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(RL, 13, Q)));  // 16 = 二倍点 Y 坐标平方 19^2 mod 23 = 16
  ShowMessage(IntToStr(Int64RationalPolynomialGaloisGetValue(RR, 13, Q)));  // 16 = 二倍点 X 坐标 5^3 + 5 + 1 mod 23 = 16

  // 如果把 X Y 二倍点公式的模逆多项式求出来，会不会相等？但没有本原多项式，完全没法求逆

  P.Free;
  T.Free;
  RL.Free;
  RR.Free;
  Y2.Free;
  Y.Free;
  X.Free;
end;

procedure TFormPolynomial.btnCheckDivisionPolynomialZeroClick(
  Sender: TObject);
var
  F: TCnInt64Polynomial;
  A, B, Q, V: Int64;
begin
  // 拿椭圆曲线里 nP = 0 的点的坐标 x y，验证 fn(x) 是否等于 0
  // 要找 2 3 4 5 6 的例子

  F := TCnInt64Polynomial.Create;
  // F29 下的 Y^2 = X^3 + 6X + 1，阶为 24，有 2 3 4 的例子，后面也有 6 8 12 24

  A := 6; B := 1; Q := 29;
  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 2, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 25, Q);  // 25, 0 是二阶点
  ShowMessage(IntToStr(V));                      // 2 不是 0，正常

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 3, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 18, Q);  // 18, 5 是三阶点
  ShowMessage(IntToStr(V));                      // 是 0

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 4, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 20, Q);  // 20, 1 是四阶点
  ShowMessage(IntToStr(V));                      // 是 0

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 6, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 9, Q);   // 9, 28 是六阶点
  ShowMessage(IntToStr(V));                      // 是 0

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 8, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 7, Q);   // 7, 26 是八阶点
  ShowMessage(IntToStr(V));                      // 是 0

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 12, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 24, Q);  // 24, 22 是十二阶点
  ShowMessage(IntToStr(V));                      // 是 0

  // F23 下的 Y^2 = X^3 + X + 9，阶为 20，有 2 4 5 的例子，后面也有 10 20

  A := 1; B := 9; Q := 23;
  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 2, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 8, Q);   // 8, 0 是二阶点
  ShowMessage(IntToStr(V));                      // 2 不是 0，正常

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 4, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 5, Q);   // 5, 22 是四阶点
  ShowMessage(IntToStr(V));                      // 是 0

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 5, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 3, Q);   // 3, 4 是五阶点
  ShowMessage(IntToStr(V));                      // 是 0

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 10, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 16, Q);  // 16, 2 是十阶点
  ShowMessage(IntToStr(V));                      // 是 0

  Int64PolynomialGaloisCalcDivisionPolynomial(A, B, 20, F, Q);
  ShowMessage(F.ToString);
  V := Int64PolynomialGaloisGetValue(F, 6, Q);   // 6, 22 是二十阶点
  ShowMessage(IntToStr(V));                      // 是 0
  F.Free;
end;

procedure TFormPolynomial.btnCalcSimpleEccClick(Sender: TObject);
var
  List: TStrings;

  procedure CalcEccPoints(A, B, Q: Int64; AList: TStrings);
  var
    Ecc: TCnInt64Ecc;
    I, J, K: Integer;
    P, T: TCnInt64EccPoint;
  begin
    AList.Clear;
    Ecc := TCnInt64Ecc.Create(A, B, Q, 0, 0, Q);
    for J := 0 to Q - 1 do
    begin
      P.X := J;
      for I := 0 to Q - 1 do
      begin
        P.Y := I;
        if Ecc.IsPointOnCurve(P) then
        begin
          // 找到一个点，然后验证它乘多少到 0
          for K := 1 to 2 * Q do
          begin
            T := P;
            Ecc.MultiplePoint(K, T);
            if (T.X = 0) and (T.Y = 0) then
            begin
              AList.Add(Format('(%d, %d) * %d = 0', [P.X, P.Y, K]));
              Break;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  List := TStringList.Create;
  mmoEcc.Clear;
  CalcEccPoints(6, 1, 29, List); // 有 2 3 4 6 8 12 24 阶点
  ShowMessage(List.Text);
  mmoEcc.Lines.AddStrings(List);
  mmoEcc.Lines.Add('');

  CalcEccPoints(1, 9, 23, List); // 有 2 4 5 10 20 阶点
  ShowMessage(List.Text);
  mmoEcc.Lines.AddStrings(List);

  List.Free;
end;

procedure TFormPolynomial.btnCheckRationalAddClick(Sender: TObject);
var
  X, Y, M2X, M2Y, M3X, M3Y, M4X, M4Y, M5X, M5Y: TCnInt64RationalPolynomial;
  DP: TCnInt64Polynomial;
begin
  // 检查一倍点表达式和二倍点表达式相加，结果是否等于三倍点
  // 一倍点 (x, 1 * y)，二倍点用 RationalMultiplePoint 算

  X := TCnInt64RationalPolynomial.Create;
  Y := TCnInt64RationalPolynomial.Create;
  M2X := TCnInt64RationalPolynomial.Create;
  M2Y := TCnInt64RationalPolynomial.Create;
  M3X := TCnInt64RationalPolynomial.Create;
  M3Y := TCnInt64RationalPolynomial.Create;
  M4X := TCnInt64RationalPolynomial.Create;
  M4Y := TCnInt64RationalPolynomial.Create;
  M5X := TCnInt64RationalPolynomial.Create;
  M5Y := TCnInt64RationalPolynomial.Create;
  DP := TCnInt64Polynomial.Create;

  Int64PolynomialGaloisCalcDivisionPolynomial(6, 1, 5, DP, 29); // 算得 5 阶可除多项式
  ShowMessage('DP5: ' + DP.ToString);

  X.Denominator.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.Nominator.SetOne;
  Y.Denominator.SetCoefficents([1]);     // ( x/1, 1/1 *y)

  ShowMessage('P2:');
  TCnInt64PolynomialEcc.RationalPointAddPoint(X, Y, X, Y, M2X, M2Y, 6, 1, 29, DP);
  ShowMessage(M2X.ToString);  // 应该输出 7x^0,21x^1,17x^2,0x^3,1x^4 / 4x^0,24x^1,0x^2,4x^3
  ShowMessage(M2Y.ToString);  // 应该输出 8x^0,5x^1,23x^2,20x^3,1x^4,0x^5,1x^6 / (8x^0,19x^1,0x^2,8x^3) * y
  // 分母也就是再乘以 y，并将 y^2 替换成 x^3 + 6x + 1，得到 8x^6+9x^4+16x^3+27x^2+9x+8 结果对了

  ShowMessage('P3:');
  TCnInt64PolynomialEcc.RationalPointAddPoint(X, Y, M2X, M2Y, M3X, M3Y, 6, 1, 29, DP);
  ShowMessage(M3X.ToString);  // 应该输出 24x^0,8x^1,21x^2,21x^3,18x^4,3x^5,16x^6,27x^7,13x^8,6x^9,1x^10,18x^11 / 27x^0,6x^1,20x^2,19x^3,11x^4,27x^5,2x^6,16x^7,12x^8,2x^9,3x^10,8x^11 结果对了
  ShowMessage(M3Y.ToString);  // 应该输出 18x^0,28x^1,17x^2,16x^3,0x^4,21x^5,10x^6,14x^7,10x^8,12x^9,23x^10,27x^11 / 6x^0,25x^1,25x^2,0x^3,25x^4,9x^5,3x^6,25x^7,6x^8,9x^9,14x^10,1x^11 结果虽然对不上号但经过验算是相等的

  ShowMessage('P4:');
  TCnInt64PolynomialEcc.RationalPointAddPoint(X, Y, M3X, M3Y, M4X, M4Y, 6, 1, 29, DP);
  ShowMessage(M4X.ToString);
  ShowMessage(M4Y.ToString);  // 不一致但相等，略

  ShowMessage('P5:');
  TCnInt64PolynomialEcc.RationalPointAddPoint(X, Y, M4X, M4Y, M5X, M5Y, 6, 1, 29, DP);
  ShowMessage(M5X.ToString);  // 应该输出 0
  ShowMessage(M5Y.ToString);

  DP.Free;
  X.Free;
  Y.Free;
  M2X.Free;
  M2Y.Free;
  M3X.Free;
  M3Y.Free;
  M4X.Free;
  M4Y.Free;
  M5X.Free;
  M5Y.Free;
end;

procedure TFormPolynomial.btnTestPiXPolynomialClick(Sender: TObject);
var
  DP, X, Y, Pi1X, Pi1Y, Pi2X, Pi2Y: TCnInt64Polynomial;
  RX, RY: TCnInt64RationalPolynomial;
//  Pi2RX, Pi2RY, R2X, R2Y, S2X, S2Y: TCnInt64RationalPolynomial;
begin
{
  对于 F97 上的椭圆曲线 Y2=X3+31X-12 的五阶扭点，注意系数只要针对 97 同余就相等
  计算 π(x^97, y^97) 与　π(x^97^2, y^97^2) 与 2 * (x, 1*y)

π(x, y) =
[47 x^11 + 11 x^10 - 16 x^9 + 8 x^8 + 44 x^7 + 8 x^6 + 10 x^5 + 12 x^4 - 40 x^3 + 42 x^2 + 11 x + 26,
(6 x^11 + 45 x^10 + 34 x^9 + 28 x^8 - 11 x^7 + 3 x^6 - 3 x^5 + 2 x^4 - 39 x^3 -^48 x^2 - x - 9)y].

π^2(x, y) =
[-17 x^11 + 2 x^10 - 25 x^9 - x^8 + 28 x^7 + 31 x^6 + 25 x^5 - 32 x^4 + 45 x^3 + 26 x^2 + 36 x + 60,
(34 x^11 + 35 x^10 - 8 x^9 - 11 x^8 - 48 x^7 + 34 x^6 - 8 x^5 - 37 x^4 - 21 x^3 + 40 x^2 + 11 x + 48)y].

2 *(x, y) =
[22 x^11 + 17 x^10 + 18 x^9 + 40 x^8 + 41 x^7 - 13 x^6 + 30 x^5 + 11 x^4 - 38 x^3 + 7 x^2 + 20 x + 17,
(-11 x^10 - 17 x^9 - 48 x^8 - 12 x^7 + 17 x^6 + 44 x^5 - 10 x^4 + 8 x^3 + 38 x^2 + 25 x + 24)y].

π^2(x, y) + [2]P =   (就这个不对！如果在 Ring 5 中计算的话，5 阶可除多项式最高 12 次方，所以上述均最高只有 11 次，但和为何冒出了 14 次？)
[-14 x^14 + 15 x^13 - 20 x^12 - 43 x^11 - 10 x^10 - 27 x^9 + 5 x^7 + 11 x^6 + 45 x^5 - 17 x^4 + 30 x^3 - 2 x^2 + 35 x - 46,
(-11 x^14 - 35 x^13 - 26 x^12 - 21 x^11 + 25 x^10 + 23 x^9 + 4 x^8 - 24 x^7 + 9 x^6 + 43 x^5 - 47 x^4 + 26 x^3 + 19 x^2 - 40 x - 32)y].

最后和点的 x 坐标和 π的 1 倍点的 x 坐标有最大公因式 <> 1，y 也一样，所以得到 t5 = 1

  用例来源于一个 PPT

  Counting points on elliptic curves over Fq
           Christiane Peters
        DIAMANT-Summer School on
 Elliptic and Hyperelliptic Curve Cryptography
          September 17, 2008
}

  DP := TCnInt64Polynomial.Create;
  Pi1X := TCnInt64Polynomial.Create;
  Pi1Y := TCnInt64Polynomial.Create;
  Pi2X := TCnInt64Polynomial.Create;
  Pi2Y := TCnInt64Polynomial.Create;

  X := TCnInt64Polynomial.Create;
  Y := TCnInt64Polynomial.Create([-12, 31, 0, 1]);

  Int64PolynomialGaloisCalcDivisionPolynomial(31, -12, 5, DP, 97);

  X.MaxDegree := 1;
  X[1] := 1;                 // x
  Int64PolynomialGaloisPower(Pi1X, X, 97, 97, DP);
  ShowMessage(Pi1X.ToString);               // 得到正确结果，Ring 几内计算就是 mod f几

  Int64PolynomialGaloisPower(Pi1Y, Y, (97 - 1) div 2, 97, DP);
  ShowMessage(Pi1Y.ToString);               // 得到正确结果，y^q = y^q-1 * y = (x3+Ax+B)^((q-1)/2) * y

  X.MaxDegree := 1;
  X[1] := 1;                 // x
  Int64PolynomialGaloisPower(Pi2X, X, 97 * 97, 97, DP);
  ShowMessage(Pi2X.ToString);         // 得到基本正确的结果，Ring 几内计算就是 mod f几，原用例最后一项常数项可能有错

  Y.SetCoefficents([-12, 31, 0, 1]);
  Int64PolynomialGaloisPower(Pi2Y, Y, (97 * 97 - 1) div 2, 97, DP);
  ShowMessage(Pi2Y.ToString);               // 得到正确结果，y^q^2 = y^q^2-1 * y = (x3+Ax+B)^((q^2-1)/2) * y

  RX := TCnInt64RationalPolynomial.Create;
  RY := TCnInt64RationalPolynomial.Create;
  TCnInt64PolynomialEcc.RationalMultiplePoint(2, RX, RY, 31, -12, 97);
  // ShowMessage(RX.ToString);
  // ShowMessage(RY.ToString);              // 得到 2P 的 X 和 Y 坐标的有理形式

  Int64PolynomialGaloisModularInverse(X, RX.Denominator, DP, 97);
  Int64PolynomialGaloisMul(X, X, RX.Nominator, 97, DP);
  ShowMessage(X.ToString);               // 用模逆多项式将 2P 的 X 坐标转换为多项式，得到正确结果

  Int64PolynomialGaloisModularInverse(Y, RY.Denominator, DP, 97);
  Int64PolynomialGaloisMul(Y, Y, RY.Nominator, 97, DP);
  ShowMessage(Y.ToString);               // 用模逆多项式将 2P 的 Y 坐标转换为多项式，得到正确结果

  // 不能简单相加，得判断两个 X 是否相等，直接判断模系数等式？
  if Int64PolynomialGaloisEqual(Pi2X, X, 97) then
    ShowMessage('π^2 (x) == 2 * P (x)')
  else
    ShowMessage('π^2 (x) <> 2 * P (x)');

  // 不能简单相加，得判断两个 Y 是否相等，直接判断模系数等式？
  if Int64PolynomialGaloisEqual(Pi2Y, Y, 97) then
    ShowMessage('π^2 (y) == 2 * P (y)')
  else
    ShowMessage('π^2 (y) <> 2 * P (y)');

  RX.Free;
  RY.Free;
  Pi1X.Free;
  Pi1Y.Free;
  Pi2X.Free;
  Pi2Y.Free;
  DP.Free;
  X.Free;
  Y.Free;
end;

procedure TFormPolynomial.btnTestGaloisDivTimeClick(Sender: TObject);
var
  T: Cardinal;
  P1, P2, R, M: TCnInt64Polynomial;
begin
  // X^97^2 - X div 5X^12+79X^10+96X^9+72X^8+57X^7+58X^6+7X^5+3X^4+83X^3+26X^2+40X+47 的耗时
  P1 := TCnInt64Polynomial.Create;
  P2 := TCnInt64Polynomial.Create([47,40,26,83,3,7,58,57,72,96,79,0,5]);
  P1.MaxDegree := 97 * 97;
  P1[P1.MaxDegree] := 1;
  P1[1] := -1;   // P1 := X^97^2 - X

  R := TCnInt64Polynomial.Create;
  M := TCnInt64Polynomial.Create;
  T := GetTickCount;
  Int64PolynomialGaloisDiv(R, M, P1, P2, 97);  // 优化 ShiftLeft 的批量插入，由 90 多秒优化到 10 秒左右
  T := GetTickCount - T;

  ShowMessage(IntToStr(T) + ': ' + M.ToString); // 80x^11 + 2x^10 + 72^9 + 96x^8 + 28x^7 + 31x^6 + 25x^5 + 65x^4 + 45x^3 + 26^2 + 35x + 34

  R.Free;
  M.Free;
  P2.Free;
  P1.Free;
end;

procedure TFormPolynomial.btnTestGaloisCalcClick(Sender: TObject);
var
  A, B, DP, R: TCnInt64Polynomial;
begin
  // 8x^11,15x^10,23x^9,23x^8,27x^7,9x^6,25x^5,19x^4,6x^3,23x^2,5x^1,22x^0  * 1 0 6 1 mod DP5 ?= 21X^11+5X^10+12X^9+4X^8+5X^7+23X^6+17X^5+11X^4+22X^3+23X^2+16X+6
  A := TCnInt64Polynomial.Create([1, 6, 0 ,1]);
  B := TCnInt64Polynomial.Create([22,5,23,6,19,25,9,27,23,23,15,8]);
  DP := TCnInt64Polynomial.Create;
  Int64PolynomialGaloisCalcDivisionPolynomial(6, 1, 5, DP, 29); // 算得 5 阶可除多项式
  R := TCnInt64Polynomial.Create;
  Int64PolynomialGaloisMul(R, B, A, 29, DP);

  ShowMessage(R.ToString);
  R.Free;
  DP.Free;
  B.Free;
  A.Free;
end;

procedure TFormPolynomial.btnTestGaloisEqualClick(Sender: TObject);
var
  A, B: TCnInt64RationalPolynomial;
  DP, TI1, TI2: TCnInt64Polynomial;
begin
  A := TCnInt64RationalPolynomial.Create;
  B := TCnInt64RationalPolynomial.Create;
  DP := TCnInt64Polynomial.Create;
  Int64PolynomialGaloisCalcDivisionPolynomial(6, 1, 5, DP, 29); // 算得 5 阶可除多项式

  // 比较 '6X^11+20X^10+13X^9+20X^8+15X^7+X^6+25X^5+2X^4+13X^3+7X^2+25X+13 / 21X^11+5X^10+12X^9+4X^8+5X^7+23X^6+17X^5+11X^4+22X^3+23X^2+16X+6'
  // 和 27x^11,23x^10,12x^9,10x^8,14x^7,10x^6,21x^5,0x^4,16x^3,17x^2,28x^1,18x^0 / 1x^11,14x^10,9x^9,6x^8,25x^7,3x^6,9x^5,25x^4,0x^3,25x^2,25x^1,6x^0
  A.Nominator.SetCoefficents([13,25,7,13,2,25,1,15,20,13,20,6]);
  A.Denominator.SetCoefficents([6,16,23,22,11,17,23,5,4,12,5,21]);

  B.Nominator.SetCoefficents([18,28,17,16,0,21,10,14,10,12,23,27]);
  B.Denominator.SetCoefficents([6,25,25,0,25,9,3,25,6,9,14,1]);

  TI1 := TCnInt64Polynomial.Create;
  TI2 := TCnInt64Polynomial.Create;

  Int64PolynomialGaloisMul(TI1, A.Nominator, B.Denominator, 29, DP);
  Int64PolynomialGaloisMul(TI2, A.Denominator, B.Nominator, 29, DP);

  if Int64PolynomialGaloisEqual(TI1, TI2, 29) then
    ShowMessage('Equal')  // 应该得到 Equal
  else
    ShowMessage('NOT Equal');

  TI2.Free;
  TI1.Free;

  B.Free;
  A.Free;
end;

procedure TFormPolynomial.btnTestHugeDivClick(Sender: TObject);
var
  A, B: TCnInt64Polynomial;
begin
  // '1426381536X^2+998173947X+1548285621' ^ 2 div X^3+7X+1
  A := TCnInt64Polynomial.Create([1548285621, 998173947, 1426381536]);
  B := TCnInt64Polynomial.Create([1, 0, 7, 1]);

  Int64PolynomialGaloisMul(A, A, A, 3037000493, B);
  ShowMessage(A.ToString);

  B.Free;
  A.Free;
end;

procedure TFormPolynomial.btnTestHugeDiv2Click(Sender: TObject);
var
  A, B: TCnInt64Polynomial;
begin
  // '25X^3+3855419515X+4165899502' mod '3352796231X^2+4242209446X+55674432'
  A := TCnInt64Polynomial.Create;
  A.MaxDegree := 3;
  A[0] := 4165899502;
  A[1] := 3855419515;
  A[2] := 0;
  A[3] := 25;
  B := TCnInt64Polynomial.Create;
  B.MaxDegree := 2;
  B[0] := 55674432;
  B[1] := 4242209446;
  B[2] := 3352796231;

  Int64PolynomialGaloisMod(B, A, B, 4294967291);
  ShowMessage(B.ToString);

  B.Free;
  A.Free;
end;

procedure TFormPolynomial.btnTestHugeDiv3Click(Sender: TObject);
var
  A, B: TCnInt64Polynomial;
begin
  // 3632376218X^2+3632376218X+1810096466' mod '488892432X+2787301319'
  A := TCnInt64Polynomial.Create;
  A.MaxDegree := 2;
  A[0] := 1810096466;
  A[1] := 3632376218;
  A[2] := 3632376218;
  B := TCnInt64Polynomial.Create;
  B.MaxDegree := 1;
  B[0] := 2787301319;
  B[1] := 488892432;

  Int64PolynomialGaloisMod(B, A, B, 4294967291);
  ShowMessage(B.ToString);

  B.Free;
  A.Free;
end;

procedure TFormPolynomial.btnTestPowerModClick(Sender: TObject);
var
  LDP, P1, P2: TCnInt64Polynomial;
  Q: Int64;
begin
  Q := 13;

  LDP := TCnInt64Polynomial.Create;
  P1 := TCnInt64Polynomial.Create;
  P2 := TCnInt64Polynomial.Create;

  P1.SetCoefficents([0, 1]); // x
  P2.SetCoefficents([0, 1]); // x

  Int64PolynomialGaloisCalcDivisionPolynomial(2, 1, 3, LDP, Q);
  ShowMessage(LDP.ToString);  // 3x^4 + 12x^2 + 12x + 9

  Int64PolynomialGaloisPower(P1, P1, Q * Q, Q, LDP);
  ShowMessage(P1.ToString);  // X

  Int64PolynomialGaloisPower(P2, P2, Q, Q, LDP);
  Int64PolynomialGaloisPower(P2, P2, Q, Q, LDP);
  ShowMessage(P2.ToString);  // X

  P2.Free;
  P1.Free;
  LDP.Free;
end;

procedure TFormPolynomial.btnBNPToStringClick(Sender: TObject);
var
  BP: TCnBigNumberPolynomial;
begin
  BP := TCnBigNumberPolynomial.Create([23, 4, -45, 6, -78, 23, 34, 1, 0, -34, 4]);
  edtBNPolynomial.Text := BP.ToString;
  BP.Free;
end;

procedure TFormPolynomial.btnBP2RandClick(Sender: TObject);
var
  I, D: Integer;
  T: TCnBigNumber;
begin
  D := StrToIntDef(edtBP1Deg.Text, 10);
  FBP1.Clear;
  Randomize;
  for I := 0 to D do
  begin
    T := TCnBigNumber.Create;
    T.SetDec(IntToStr(Random(256000) - 128000));
    FBP1.Add(T);
  end;
  mmoBP1.Lines.Text := FBP1.ToString;
end;

procedure TFormPolynomial.btnBP1RandClick(Sender: TObject);
var
  I, D: Integer;
  T: TCnBigNumber;
begin
  D := StrToIntDef(edtBP2Deg.Text, 10);
  FBP2.Clear;
  Randomize;
  for I := 0 to D do
  begin
    T := TCnBigNumber.Create;
    T.SetDec(IntToStr(Random(256000) - 128000));
    FBP2.Add(T);
  end;
  mmoBP2.Lines.Text := FBP2.ToString;
end;

procedure TFormPolynomial.btnBPAddClick(Sender: TObject);
begin
  if BigNumberPolynomialAdd(FBP3, FBP1, FBP2) then
    edtBP3.Text := BigNumberPolynomialToString(FBP3);
end;

procedure TFormPolynomial.btnBPSubClick(Sender: TObject);
begin
  if BigNumberPolynomialSub(FBP3, FBP1, FBP2) then
    edtBP3.Text := BigNumberPolynomialToString(FBP3);
end;

procedure TFormPolynomial.btnBPMulClick(Sender: TObject);
begin
  if BigNumberPolynomialMul(FBP3, FBP1, FBP2) then
    edtBP3.Text := BigNumberPolynomialToString(FBP3);
end;

procedure TFormPolynomial.btnBPDivModClick(Sender: TObject);
var
  R: TCnBigNumberPolynomial;
begin
  R := TCnBigNumberPolynomial.Create;

  if not FBP2[FBP2.MaxDegree].IsOne then
  begin
    ShowMessage('Divisor MaxDegree only Support 1, change to 1');
    FBP2[FBP2.MaxDegree].SetOne;
    mmoBP2.Lines.Text := FBP2.ToString;
  end;

  if BigNumberPolynomialDiv(FBP3, R, FBP1, FBP2) then
  begin
    edtBP3.Text := FBP3.ToString;
    ShowMessage('Remain: ' + R.ToString);
  end;

  // 验算 FIP3 * FIP2 + R
  BigNumberPolynomialMul(FBP3, FBP3, FBP2);
  BigNumberPolynomialAdd(FBP3, FBP3, R);
  ShowMessage(FBP3.ToString);
  if mmoBP1.Lines.Text = FBP3.ToString then
    ShowMessage('Equal Verified OK.');
  R.Free;
end;

procedure TFormPolynomial.btnBNTestExample1Click(Sender: TObject);
var
  X, Y, P: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
{
  用例一：
  构造一个有限域的二阶扩域 67*67，并指定其本原多项式是 u^2 + 1 = 0，
  然后在上面构造一条椭圆曲线 y^2 = x^3 + 4x + 3，选一个点 2u + 16, 30u + 39
  验证这个点在该椭圆曲线上。（注意 n 阶扩域上的椭圆曲线上的点的坐标是一对 n 次多项式）

  该俩用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.5

  具体实现就是计算(Y^2 - X^3 - A*X - B) mod Primtive，然后每个系数运算时都要 mod p
  这里 A = 4，B = 3。
  二阶扩域上，p 是素数 67，本原多项式是 u^2 + 1
}

  X := TCnBigNumberPolynomial.Create([16, 2]);
  Y := TCnBigNumberPolynomial.Create([39, 30]);
  P := TCnBigNumberPolynomial.Create([1, 0, 1]);
  Q := TCnBigNumber.Create;
  Q.SetWord(67);

  try
    BigNumberPolynomialGaloisMul(Y, Y, Y, Q, P); // Y^2 得到 62X + 18

    BigNumberPolynomialMulWord(X, 4);
    BigNumberPolynomialSub(Y, Y, X);
    BigNumberPolynomialSubWord(Y, 3);             // Y 减去了 A*X - B，得到 54X + 18
    BigNumberPolynomialNonNegativeModWord(Y, 67);

    X.SetCoefficents([16, 2]);
    BigNumberPolynomialGaloisPower(X, X, 3, Q, P);  // 得到 54X + 18

    BigNumberPolynomialGaloisSub(Y, Y, X, Q);
    BigNumberPolynomialGaloisMod(Y, Y, P, Q);    // 算出 0
    ShowMessage(Y.ToString);
  finally
    P.Free;
    Y.Free;
    X.Free;
    Q.Free;
  end;
end;

procedure TFormPolynomial.btnBNTestExample2Click(Sender: TObject);
var
  X, Y, P: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
{
  用例二：
  构造一个有限域的二阶扩域 7691*7691，并指定其本原多项式是 u^2 + 1 = 0，
  然后在上面构造一条椭圆曲线 y^2=x^3+1 mod 7691，选一个点 633u + 6145, 7372u + 109
  验证这个点在该椭圆曲线上。

  该俩用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 4.0.1

  具体实现就是计算(Y^2 - X^3 - A*X - B) mod Primtive，然后每个系数运算时都要 mod p
  这里 A = 0，B = 1
  二阶扩域上，p 是素数 7691，本原多项式是 u^2 + 1
}

  X := TCnBigNumberPolynomial.Create([6145, 633]);
  Y := TCnBigNumberPolynomial.Create([109, 7372]);
  P := TCnBigNumberPolynomial.Create([1, 0, 1]);
  Q := TCnBigNumber.Create;
  Q.SetWord(7691);

  try
    BigNumberPolynomialGaloisMul(Y, Y, Y, Q, P);

    BigNumberPolynomialSubWord(Y, 1);
    BigNumberPolynomialNonNegativeModWord(Y, 7691);

    X.SetCoefficents([6145, 633]);
    BigNumberPolynomialGaloisPower(X, X, 3, Q, P);

    BigNumberPolynomialGaloisSub(Y, Y, X, Q);
    BigNumberPolynomialGaloisMod(Y, Y, P, Q);    // 算出 0
    ShowMessage(Y.ToString);
  finally
    P.Free;
    Y.Free;
    X.Free;
    Q.Free;
  end;
end;

procedure TFormPolynomial.btnBNTestExample3Click(Sender: TObject);
var
  X, P: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
{
  用例三：
  构造一个有限域的二阶扩域 67*67，并指定其本原多项式是 u^2 + 1 = 0，
  验证：(2u + 16)^67 = 65u + 16, (30u + 39)^67 = 37u + 39

  该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.5
}

  X := TCnBigNumberPolynomial.Create([16, 2]);
  P := TCnBigNumberPolynomial.Create([1, 0, 1]);
  Q := TCnBigNumber.Create;
  Q.SetWord(67);

  try
    BigNumberPolynomialGaloisPower(X, X, Q, Q, P);
    ShowMessage(X.ToString);  // 得到 65x + 16

    X.SetCoefficents([39, 30]);
    BigNumberPolynomialGaloisPower(X, X, Q, Q, P);
    ShowMessage(X.ToString);  // 得到 37x + 39
  finally
    X.Free;
    P.Free;
    Q.Free;
  end;
end;

procedure TFormPolynomial.btnBNTestExample4Click(Sender: TObject);
var
  X, P: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
{
  用例四：
  构造一个有限域的三阶扩域 67*67*67，并指定其本原多项式是 u^3 + 2 = 0，
  验证：
  (15v^2 + 4v + 8)^67  = 33v^2 + 14v + 8, 44v^2 + 30v + 21)^67 = 3v^2 + 38v + 21
  (15v^2 + 4v + 8)^(67^2)  = 19v^2 + 49v + 8, (44v^2 + 30v + 21)^(67^2) = 20v^2 + 66v + 21
  (15v^2 + 4v + 8)^(67^3)  = 15v^2 + 4v + 8,  (44v^2 + 30v + 21)^(67^3) = 44v^2 + 30v + 21 都回到自身

  该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.5
}

  X := TCnBigNumberPolynomial.Create;
  P := TCnBigNumberPolynomial.Create([2, 0, 0, 1]);
  Q := TCnBigNumber.Create;
  Q.SetWord(67);

  try
    X.SetCoefficents([8, 4, 15]);
    BigNumberPolynomialGaloisPower(X, X, 67, Q, P);
    ShowMessage(X.ToString);  // 33x^2 + 14x + 8

    X.SetCoefficents([21, 30, 44]);
    BigNumberPolynomialGaloisPower(X, X, 67, Q, P);
    ShowMessage(X.ToString);  // 3x^2 + 38x + 21

    X.SetCoefficents([8, 4, 15]);
    BigNumberPolynomialGaloisPower(X, X, 67 * 67, Q, P);
    ShowMessage(X.ToString);  // 19x^2 + 49x + 8

    X.SetCoefficents([21, 30, 44]);
    BigNumberPolynomialGaloisPower(X, X, 67 * 67, Q, P);
    ShowMessage(X.ToString);  // 20x^2 + 66x + 21

    X.SetCoefficents([8, 4, 15]);
    BigNumberPolynomialGaloisPower(X, X, 67 * 67 * 67, Q, P);
    ShowMessage(X.ToString);  // 15x^2 + 4x + 8

    X.SetCoefficents([21, 30, 44]);
    BigNumberPolynomialGaloisPower(X, X, 67 * 67 * 67, Q, P);
    ShowMessage(X.ToString);  // 44x^2 + 30x + 21
  finally
    X.Free;
    P.Free;
    Q.Free;
  end;
end;

procedure TFormPolynomial.btnBNGaloisTestGcdClick(Sender: TObject);
var
  Q: TCnBigNumber;
begin
// GCD 例子一：
// F11 扩域上的 x^2 + 8x + 7 和 x^3 + 7x^2 + x + 7 的最大公因式是 x + 7
  Q := TCnBigNumber.Create;
  Q.SetWord(11);

  FBP1.SetCoefficents([7, 8, 1]);
  FBP2.SetCoefficents([7, 1, 7, 1]);  // 而和 [7, 1, 2, 1] 则互素
  if BigNumberPolynomialGaloisGreatestCommonDivisor(FBP3, FBP1, FBP2, Q) then
    ShowMessage(FBP3.ToString);

// GCD 例子二：
// F2 扩域上的 x^6 + x^5 + x^4 + x^3 + x^2 + x + 1 和 x^4 + x^2 + x + 1 的最大公因式是 x^3 + x^2 + 1
  FBP1.SetCoefficents([1,1,1,1,1,1,1]);
  FBP2.SetCoefficents([1,1,1,0,1]);
  Q.SetWord(2);
  if BigNumberPolynomialGaloisGreatestCommonDivisor(FBP3, FBP1, FBP2, Q) then
    ShowMessage(FBP3.ToString);

  Q.Free;
end;

procedure TFormPolynomial.btnBNTestGaloisMIClick(Sender: TObject);
var
  Q: TCnBigNumber;
begin
// Modulus Inverse 例子：
// F3 的扩域上的本原多项式 x^3 + 2x + 1 有 x^2 + 1 的模逆多项式为 2x^2 + x + 2
  FBP1.SetCoefficents([1, 0, 1]);
  FBP2.SetCoefficents([1, 2, 0, 1]);
  Q := TCnBigNumber.Create;
  Q.SetWord(3);

  BigNumberPolynomialGaloisModularInverse(FBP3, FBP1, FBP2, Q);
    ShowMessage(FBP3.ToString);

  Q.Free;
end;

procedure TFormPolynomial.btnBNGF28Test1Click(Sender: TObject);
var
  BP: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
  FBP1.SetCoefficents([1,1,1,0,1,0,1]); // 57
  FBP2.SetCoefficents([1,1,0,0,0,0,0,1]); // 83
  FBP3.SetCoefficents([1,1,0,1,1,0,0,0,1]); // 本原多项式

  BP := TCnBigNumberPolynomial.Create;
  Q := TCnBigNumber.Create;
  Q.SetWord(2);
  BigNumberPolynomialGaloisMul(BP, FBP1, FBP2, Q, FBP3);
  ShowMessage(BP.ToString);  // 得到 1,0,0,0,0,0,1,1
  BP.Free;
end;

procedure TFormPolynomial.btnBNPolyGcdClick(Sender: TObject);
begin
  if not FBP1[FBP1.MaxDegree].IsOne and not FBP2[FBP2.MaxDegree].IsOne then
  begin
    ShowMessage('Divisor MaxDegree only Support 1, change to 1');
    FBP1[FBP1.MaxDegree].SetOne;
    mmoBP1.Lines.Text := FBP1.ToString;
    FBP2[FBP2.MaxDegree].SetOne;
    mmoBP2.Lines.Text := FBP2.ToString;
  end;

//  FIP1.SetCoefficents([-5, 2, 0, 3]);
//  FIP2.SetCoefficents([-1, -2, 0, 3]);
  if BigNumberPolynomialGreatestCommonDivisor(FBP3, FBP1, FBP2) then
    edtBP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnBNTestGaloisDivClick(Sender: TObject);
var
  A, B, C, D: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
  // GF13 上 (2x^2+3x+3) div (6x) 应该等于 9x + 7 余 3
  A := TCnBigNumberPolynomial.Create([3,3,2]);
  B := TCnBigNumberPolynomial.Create([0,6]);
  C := TCnBigNumberPolynomial.Create;
  D := TCnBigNumberPolynomial.Create;
  Q := TCnBigNumber.Create;
  Q.SetWord(13);
  BigNumberPolynomialGaloisDiv(C, D, A, B, Q);
  ShowMessage(C.ToString); // 9x + 7
  ShowMessage(D.ToString); // 3
  A.Free;
  B.Free;
  C.Free;
  D.Free;
  Q.Free;
end;

procedure TFormPolynomial.btnBNTestGaloisCalcClick(Sender: TObject);
var
  A, B, DP, R: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
  // 8x^11,15x^10,23x^9,23x^8,27x^7,9x^6,25x^5,19x^4,6x^3,23x^2,5x^1,22x^0  * 1 0 6 1 mod DP5 ?= 21X^11+5X^10+12X^9+4X^8+5X^7+23X^6+17X^5+11X^4+22X^3+23X^2+16X+6
  A := TCnBigNumberPolynomial.Create([1, 6, 0 ,1]);
  B := TCnBigNumberPolynomial.Create([22,5,23,6,19,25,9,27,23,23,15,8]);
  DP := TCnBigNumberPolynomial.Create;
  Q := TCnBigNumber.Create;
  Q.SetWord(29);

  BigNumberPolynomialGaloisCalcDivisionPolynomial(6, 1, 5, DP, Q); // 算得 5 阶可除多项式
  R := TCnBigNumberPolynomial.Create;
  BigNumberPolynomialGaloisMul(R, B, A, Q, DP);

  ShowMessage(R.ToString);
  R.Free;
  DP.Free;
  B.Free;
  A.Free;
  Q.Free;
end;

procedure TFormPolynomial.btnBNTestHugeDiv1Click(Sender: TObject);
var
  A, B: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
  // '1426381536X^2+998173947X+1548285621' ^ 2 div X^3+7X+1
  A := TCnBigNumberPolynomial.Create([1548285621, 998173947, 1426381536]);
  B := TCnBigNumberPolynomial.Create([1, 0, 7, 1]);
  Q := TCnBigNumber.Create;
  Q.SetInt64(3037000493);

  BigNumberPolynomialGaloisMul(A, A, A, Q, B);
  ShowMessage(A.ToString);

  B.Free;
  A.Free;
  Q.Free;
end;

procedure TFormPolynomial.btnBNTestHugeDiv2Click(Sender: TObject);
var
  A, B: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
  // '25X^3+3855419515X+4165899502' mod '3352796231X^2+4242209446X+55674432'
  A := TCnBigNumberPolynomial.Create;
  A.MaxDegree := 3;
  A[0].SetInt64(4165899502);
  A[1].SetInt64(3855419515);
  A[2].SetInt64(0);
  A[3].SetInt64(25);
  B := TCnBigNumberPolynomial.Create;
  B.MaxDegree := 2;
  B[0].SetInt64(55674432);
  B[1].SetInt64(4242209446);
  B[2].SetInt64(3352796231);

  Q := TCnBigNumber.Create;
  Q.SetInt64(4294967291);
  BigNumberPolynomialGaloisMod(B, A, B, Q);
  ShowMessage(B.ToString);

  B.Free;
  A.Free;
  Q.Free;
end;

procedure TFormPolynomial.btnBNTestHugeDiv3Click(Sender: TObject);
var
  A, B: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
  // 3632376218X^2+3632376218X+1810096466' mod '488892432X+2787301319'
  A := TCnBigNumberPolynomial.Create;
  A.MaxDegree := 2;
  A[0].SetInt64(1810096466);
  A[1].SetInt64(3632376218);
  A[2].SetInt64(3632376218);
  B := TCnBigNumberPolynomial.Create;
  B.MaxDegree := 1;
  B[0].SetInt64(2787301319);
  B[1].SetInt64(488892432);
  Q := TCnBigNumber.Create;
  Q.SetInt64(4294967291);

  BigNumberPolynomialGaloisMod(B, A, B, Q);
  ShowMessage(B.ToString);

  B.Free;
  A.Free;
  Q.Free;
end;

procedure TFormPolynomial.btnBNTestPowerModClick(Sender: TObject);
var
  LDP, P1, P2: TCnBigNumberPolynomial;
  Q, Q2: TCnBigNumber;
begin
  Q := TCnBigNumber.Create;
  Q.SetInteger(13);
  Q2 := TCnBigNumber.Create;
  BigNumberMul(Q2, Q, Q);

  LDP := TCnBigNumberPolynomial.Create;
  P1 := TCnBigNumberPolynomial.Create;
  P2 := TCnBigNumberPolynomial.Create;

  P1.SetCoefficents([0, 1]); // x
  P2.SetCoefficents([0, 1]); // x

  BigNumberPolynomialGaloisCalcDivisionPolynomial(2, 1, 3, LDP, Q);
  ShowMessage(LDP.ToString);

  BigNumberPolynomialGaloisPower(P1, P1, Q2, Q, LDP);
  ShowMessage(P1.ToString);  // X

  BigNumberPolynomialGaloisPower(P2, P2, Q, Q, LDP);
  BigNumberPolynomialGaloisPower(P2, P2, Q, Q, LDP);
  ShowMessage(P2.ToString);  // X

  P2.Free;
  P1.Free;
  LDP.Free;
  Q.Free;
end;

procedure TFormPolynomial.btnBNTestGaloisDivTimeClick(Sender: TObject);
var
  T: Cardinal;
  P1, P2, R, M: TCnBigNumberPolynomial;
  Q: TCnBigNumber;
begin
  // X^97^2 - X div 5X^12+79X^10+96X^9+72X^8+57X^7+58X^6+7X^5+3X^4+83X^3+26X^2+40X+47 的耗时
  P1 := TCnBigNumberPolynomial.Create;
  P2 := TCnBigNumberPolynomial.Create([47,40,26,83,3,7,58,57,72,96,79,0,5]);
  P1.MaxDegree := 97 * 97;
  P1[P1.MaxDegree].SetOne;
  P1[1].SetOne;
  P1[1].Negate;   // P1 := X^97^2 - X

  R := TCnBigNumberPolynomial.Create;
  M := TCnBigNumberPolynomial.Create;
  Q := TCnBigNumber.Create;
  Q.SetInteger(97);

  T := GetTickCount;
  BigNumberPolynomialGaloisDiv(R, M, P1, P2, Q);
  T := GetTickCount - T;

  ShowMessage(IntToStr(T) + ': ' + M.ToString); // 80x^11 + 2x^10 + 72^9 + 96x^8 + 28x^7 + 31x^6 + 25x^5 + 65x^4 + 45x^3 + 26^2 + 35x + 34
  // 耗时 70 多秒

  R.Free;
  M.Free;
  P2.Free;
  P1.Free;
  Q.Free;
end;

procedure TFormPolynomial.btnBNTestMI1Click(Sender: TObject);
var
  Q: TCnBigNumber;
begin
  Q := TCnBigNumber.Create;
  Q.SetInteger(13);

  FBP1.SetCoefficents([1,2,0,1]);
  FBP2.SetCoefficents([3,4,4,0,1]);  // 本原多项式
  BigNumberPolynomialGaloisModularInverse(FBP3, FBP1, FBP2, Q);
  ShowMessage(FBP3.ToString);  // 得到 5x^3+6x+2

  BigNumberPolynomialGaloisMul(FBP3, FBP3, FBP1, Q, FBP2); // 乘一下验算看是不是得到 1
  ShowMessage(FBP3.ToString);

  FBP1.SetCoefficents([4,8,0,4]);
  FBP2.SetCoefficents([9,12,12,0,3]);
  BigNumberPolynomialGaloisModularInverse(FBP3, FBP1, FBP2, Q);
  ShowMessage(FBP3.ToString);  // 得到 11x^3+8x+7

  Q.Free;
end;

procedure TFormPolynomial.btnBNTestEuclid2Click(Sender: TObject);
var
  A, B, X, Y: TCnBigNumberPolynomial;
begin
  FQ.SetWord(3);

  A := TCnBigNumberPolynomial.Create([1,0,1]);
  B := TCnBigNumberPolynomial.Create([1, 1]);
  X := TCnBigNumberPolynomial.Create;
  Y := TCnBigNumberPolynomial.Create;

  // 求 6x * X + 3 * Y = 1 mod 13 的解，得到 0，9
  BigNumberPolynomialGaloisExtendedEuclideanGcd(A, B, X, Y, FQ);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  A.Free;
  B.Free;
  X.Free;
  Y.Free;
end;

procedure TFormPolynomial.btnBNTestGaloisMulModClick(Sender: TObject);
var
  P, Q, H: TCnBigNumberPolynomial;
begin
  FQ.SetWord(13);
  P := TCnBigNumberPolynomial.Create([11,0,6,12]);
  Q := TCnBigNumberPolynomial.Create([2,4,0,2]);
  H := TCnBigNumberPolynomial.Create([9,12,12,0,3]);
  BigNumberPolynomialGaloisMul(P, P, Q, FQ, H);
  ShowMessage(P.ToString); // 4x3+6x2+5x+10

  H.Free;
  Q.Free;
  P.Free;
end;

procedure TFormPolynomial.btnBNTestEuclid3Click(Sender: TObject);
var
  A, B, X, Y: TCnBigNumberPolynomial;
begin
  FQ.SetWord(13);

  A := TCnBigNumberPolynomial.Create([3,3,2]);
  B := TCnBigNumberPolynomial.Create([0,6]);
  X := TCnBigNumberPolynomial.Create;
  Y := TCnBigNumberPolynomial.Create;

  // 求 2x2+3x+3 * X - 6x * Y = 1 mod 13 的解，应该得到 9 和 10x+2
  BigNumberPolynomialGaloisExtendedEuclideanGcd(A, B, X, Y, FQ);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  A.Free;
  B.Free;
  X.Free;
  Y.Free;
end;

procedure TFormPolynomial.btnBNEccDivisionPolyClick(Sender: TObject);
var
  DP: TCnBigNumberPolynomial;
  P, V, X1, X2: TCnBigNumber;
  A, B: Int64;
begin
  // Division Polynomial 测试用例，来自 John J. McGee 的
  // 《Rene Schoof's Algorithm for Determing the Order of the Group of Points
  //    on an Elliptic Curve over a Finite Field》第 31 页
  A := 46;
  B := 74;
  P := TCnBigNumber.Create;
  P.SetWord(97);
  V := TCnBigNumber.Create;

  X1 := TCnBigNumber.Create;
  X2 := TCnBigNumber.Create;
  X1.SetWord(4);
  X2.SetWord(90);

  DP := TCnBigNumberPolynomial.Create;
  mmoBNTestDivisionPolynomials.Lines.Clear;

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 2, DP, P);
  mmoBNTestDivisionPolynomials.Lines.Add('2: === ' + DP.ToString);
  BigNumberPolynomialGaloisGetValue(V, DP, X1, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 2
  BigNumberPolynomialGaloisGetValue(V, DP, X2, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 2

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 3, DP, P);
  mmoBNTestDivisionPolynomials.Lines.Add('3: === ' + DP.ToString);
  BigNumberPolynomialGaloisGetValue(V, DP, X1, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 24
  BigNumberPolynomialGaloisGetValue(V, DP, X2, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 76

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 4, DP, P);
  mmoBNTestDivisionPolynomials.Lines.Add('4: === ' + DP.ToString);
  BigNumberPolynomialGaloisGetValue(V, DP, X1, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 0
  BigNumberPolynomialGaloisGetValue(V, DP, X2, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 14

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 5, DP, P);
  mmoBNTestDivisionPolynomials.Lines.Add('5: === ' + DP.ToString);
  BigNumberPolynomialGaloisGetValue(V, DP, X1, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 47
  BigNumberPolynomialGaloisGetValue(V, DP, X2, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 0

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 6, DP, P);
  mmoBNTestDivisionPolynomials.Lines.Add('6: === ' + DP.ToString);
  BigNumberPolynomialGaloisGetValue(V, DP, X1, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 25
  BigNumberPolynomialGaloisGetValue(V, DP, X2, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 21

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 7, DP, P);
  mmoBNTestDivisionPolynomials.Lines.Add('7: === ' + DP.ToString);
  BigNumberPolynomialGaloisGetValue(V, DP, X1, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 22
  BigNumberPolynomialGaloisGetValue(V, DP, X2, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 23

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 8, DP, P);
  mmoBNTestDivisionPolynomials.Lines.Add('8: === ' + DP.ToString);
  BigNumberPolynomialGaloisGetValue(V, DP, X1, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 0，
  BigNumberPolynomialGaloisGetValue(V, DP, X2, P);
  mmoBNTestDivisionPolynomials.Lines.Add(V.ToDec);                                    // 得到 31

  DP.Free;
  P.Free;
  V.Free;
  X2.Free;
  X1.Free;
end;

procedure TFormPolynomial.btnBNGenerateDPClick(Sender: TObject);
var
  List: TObjectList;
  I: Integer;
  A, B, P: TCnBigNumber;
begin
  List := TObjectList.Create(True);
  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  P := TCnBigNumber.Create;

  A.SetWord(46);
  B.SetWord(74);
  P.SetWord(97);

  CnGenerateGaloisDivisionPolynomials(A, B, P, 20, List);
  P.Free;
  B.Free;
  A.Free;

  mmoBNTestDivisionPolynomials.Lines.Clear;
  for I := 0 to List.Count - 1 do
    mmoBNTestDivisionPolynomials.Lines.Add(TCnBigNumberPolynomial(List[I]).ToString);

  List.Free;
end;

procedure TFormPolynomial.btnBNTestDivPoly1Click(Sender: TObject);
var
  P: TCnBigNumberPolynomial;
begin
  // 验证可除多项式的生成
  // 如在 F101 上定义的椭圆曲线: y^2 = x^3 + x + 1
  // 用例数据不完整只能认为基本通过
  // 该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.9

  P := TCnBigNumberPolynomial.Create;
  FQ.SetWord(101);

  BigNumberPolynomialGaloisCalcDivisionPolynomial(1, 1, 0, P, FQ);
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(1, 1, 1, P, FQ);
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(1, 1, 2, P, FQ);
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(1, 1, 3, P, FQ);  // 3x4 +6x2+12x+100
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(1, 1, 4, P, FQ);  // ...
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(1, 1, 5, P, FQ);  // 5x12 ... 16
  ShowMessage(P.ToString);

  P.Free;
end;

procedure TFormPolynomial.btnBNTestDivPoly2Click(Sender: TObject);
var
  P: TCnBigNumberPolynomial;
begin
  // 验证可除多项式的生成
  // 如在 F13 上定义的椭圆曲线: y^2 = x^3 + 2x + 1
  // 用例数据不完整只能认为基本通过
  // 该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.10

  P := TCnBigNumberPolynomial.Create;
  FQ.SetWord(13);

  BigNumberPolynomialGaloisCalcDivisionPolynomial(2, 1, 0, P, FQ);
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(2, 1, 1, P, FQ);
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(2, 1, 2, P, FQ);
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(2, 1, 3, P, FQ);  // 3x4 +12x2+12x+9
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(2, 1, 4, P, FQ);  // ...
  ShowMessage(P.ToString);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(2, 1, 5, P, FQ);  // 5x12 ... 6x + 7
  ShowMessage(P.ToString);

  P.Free;
end;

procedure TFormPolynomial.btnBNTestDivPolyClick(Sender: TObject);
var
  F: TCnBigNumberPolynomial;
  A, B: Integer;
  Q, V, X: TCnBigNumber;
begin
  // 拿椭圆曲线里 nP = 0 的点的坐标 x y，验证 fn(x) 是否等于 0
  // 要找 2 3 4 5 6 的例子

  F := TCnBigNumberPolynomial.Create;
  // F29 下的 Y^2 = X^3 + 6X + 1，阶为 24，有 2 3 4 的例子，后面也有 6 8 12 24

  A := 6; B := 1;
  Q := TCnBigNumber.Create;
  Q.SetWord(29);
  V := TCnBigNumber.Create;
  X := TCnBigNumber.Create;

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 2, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(25);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);  // 25, 0 是二阶点
  ShowMessage(V.ToDec);                      // 2 不是 0，正常

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 3, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(18);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);  // 18, 5 是三阶点
  ShowMessage(V.ToDec);                      // 是 0

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 4, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(20);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);  // 20, 1 是四阶点
  ShowMessage(V.ToDec);                      // 是 0

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 6, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(9);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);   // 9, 28 是六阶点
  ShowMessage(V.ToDec);                      // 是 0

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 8, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(7);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);   // 7, 26 是八阶点
  ShowMessage(V.ToDec);                      // 是 0

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 12, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(24);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);  // 24, 22 是十二阶点
  ShowMessage(V.ToDec);                      // 是 0

  // F23 下的 Y^2 = X^3 + X + 9，阶为 20，有 2 4 5 的例子，后面也有 10 20

  A := 1; B := 9;
  Q.SetWord(23);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 2, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(8);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);   // 8, 0 是二阶点
  ShowMessage(V.ToDec);                      // 2 不是 0，正常

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 4, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(5);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);   // 5, 22 是四阶点
  ShowMessage(V.ToDec);                      // 是 0

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 5, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(3);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);   // 3, 4 是五阶点
  ShowMessage(V.ToDec);                      // 是 0

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 10, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(16);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);  // 16, 2 是十阶点
  ShowMessage(V.ToDec);                      // 是 0

  BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, 20, F, Q);
  ShowMessage(F.ToString);
  X.SetWord(6);
  BigNumberPolynomialGaloisGetValue(V, F, X, Q);   // 6, 22 是二十阶点
  ShowMessage(V.ToDec);                      // 是 0

  F.Free;
  Q.Free;
  V.Free;
  X.Free;
end;

procedure TFormPolynomial.btnBNEccOnCurveClick(Sender: TObject);
var
  Ecc: TCnPolynomialEcc;
  G: TCnPolynomialEccPoint;
  PM: TCnBigNumberPolynomial;
begin
{
  用例一：
  椭圆曲线 y^2 = x^3 + 4x + 3, 如果定义在二次扩域 F67^2 上，本原多项式 u^2 + 1
  判断基点 P(2u+16, 30u+39) 在曲线上

  用例二：
  椭圆曲线 y^2 = x^3 + 4x + 3, 如果定义在三次扩域 F67^3 上，本原多项式 u^3 + 2
  判断基点 P((15v^2 + 4v + 8, 44v^2 + 30v + 21)) 在曲线上

  该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.8
}

  G := TCnPolynomialEccPoint.Create;
  G.X.SetCoefficents([16, 2]);
  G.Y.SetCoefficents([39, 30]);
  PM := TCnBigNumberPolynomial.Create;
  PM.SetCoefficents([1, 0, 1]);

  // $43 = 67
  Ecc := TCnPolynomialEcc.Create('4', '3', '43', 2, G.X, G.Y, '0', PM); // Order 未指定，先不传
  if Ecc.IsPointOnCurve(Ecc.Generator) then
    ShowMessage('Ecc 1 Generator is on Curve')
  else
    ShowMessage('Error');

  Ecc.Free;

  G.X.SetCoefficents([8, 4, 15]);
  G.Y.SetCoefficents([21, 30, 44]);
  PM.SetCoefficents([2, 0, 0, 1]);
  Ecc := TCnPolynomialEcc.Create('4', '3', '43', 3, G.X, G.Y, '0', PM); // Order 未指定，先不传
  if Ecc.IsPointOnCurve(Ecc.Generator) then
    ShowMessage('Ecc 2 Generator is on Curve')
  else
    ShowMessage('Error');

  Ecc.Free;
  PM.Free;
  G.Free;
end;

procedure TFormPolynomial.btnBNEccPointAdd1Click(Sender: TObject);
var
  Ecc: TCnPolynomialEcc;
  K, Prime: TCnBigNumber;
  P, Q, S, G: TCnPolynomialEccPoint;
  PM: TCnBigNumberPolynomial;
begin
// 有限扩域上的多项式椭圆曲线点加
// F67^2 上的椭圆曲线 y^2 = x^3 + 4x + 3 本原多项式 u^2 + 1
// 点 P(2u + 16, 30u + 39) 满足 68P + 11πP = 0
// 其中 πP 是 P 的 Frob 映射也就是 X Y 各 67 次方为用例三中的(65u + 16, 37u + 39)

// 该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.8
  G := TCnPolynomialEccPoint.Create;
  G.X.SetCoefficents([16, 2]);
  G.Y.SetCoefficents([39, 30]);
  PM := TCnBigNumberPolynomial.Create;
  PM.SetCoefficents([1,0,1]);
  K := TCnBigNumber.Create;
  Prime := TCnBigNumber.Create;

  // $43 = 67
  Ecc := TCnPolynomialEcc.Create('4', '3', '43', 2, G.X, G.Y, '0', PM); // Order 未指定，先不传

  P := TCnPolynomialEccPoint.Create;
  P.Assign(Ecc.Generator);
  K.SetWord(68);
  Ecc.MultiplePoint(K, P);
  ShowMessage(P.ToString);   // 37x+31,51x+39       // 15x+6, 63x+4

  Q := TCnPolynomialEccPoint.Create;
  Q.Assign(Ecc.Generator);
  Prime.SetWord(67);
  BigNumberPolynomialGaloisPower(Q.X, Q.X, Prime, Prime, Ecc.Primitive);
  BigNumberPolynomialGaloisPower(Q.Y, Q.Y, Prime, Prime, Ecc.Primitive);

  K.SetWord(11);
  Ecc.MultiplePoint(K, Q);
  ShowMessage(Q.ToString);   // 18x+30,41x+49       // 39x+2, 38x+48

  S := TCnPolynomialEccPoint.Create;
  Ecc.PointAddPoint(S, P, Q);
  ShowMessage(S.ToString);   // 0, 0

  K.Free;
  S.Free;
  P.Free;
  Q.Free;
  G.Free;
  PM.Free;
  Ecc.Free;
end;

procedure TFormPolynomial.btnBNEccPointAdd2Click(Sender: TObject);
var
  Ecc: TCnPolynomialEcc;
  P, Q, S, G: TCnPolynomialEccPoint;
  Prime, Prime2: TCnBigNumber;
  PM: TCnBigNumberPolynomial;
begin
// 有限扩域上的多项式椭圆曲线点加
// F67^3 上的椭圆曲线 y^2 = x^3 + 4x + 3 本原多项式 u^3 + 2
// 点 P(15v^2 + 4v + 8, 44v^2 + 30v + 21) 满足 π2P - (-11)πP + 67P = 0
// 其中 πP 是 P 的 Frob 映射也就是 X Y 各 67 次方
// πP为用例四中的(33v^2 + 14v + 8, 3v^2 + 38v + 21)
// π2P为用例四中的 67^2 次方(19v^2 + 49v + 8, 20v^2 + 66v + 21)

// 该用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.8
  Prime := TCnBigNumber.Create;
  Prime.SetWord(67);
  Prime2 := TCnBigNumber.Create;
  Prime2.SetWord(67*67);

  G := TCnPolynomialEccPoint.Create;
  G.X.SetCoefficents([8, 4, 15]);
  G.Y.SetCoefficents([21, 30, 44]);

  PM := TCnBigNumberPolynomial.Create([2, 0, 0, 1]);

  // $43 = 67
  Ecc := TCnPolynomialEcc.Create('4', '3', '43', 3, G.X, G.Y, '0', PM); // Order 未指定，先不传

  P := TCnPolynomialEccPoint.Create;
  P.Assign(Ecc.Generator);
  Ecc.MultiplePoint(Prime, P);                                        // 算 67P

  Q := TCnPolynomialEccPoint.Create;
  Q.Assign(Ecc.Generator);
  BigNumberPolynomialGaloisPower(Q.X, Q.X, Prime, Prime, Ecc.Primitive);
  BigNumberPolynomialGaloisPower(Q.Y, Q.Y, Prime, Prime, Ecc.Primitive);   // 算 πP
  Ecc.MultiplePoint(-11, Q);                                       // 算 -11πp

  S := TCnPolynomialEccPoint.Create;
  Ecc.PointSubPoint(S, P, Q);

  Q.Assign(Ecc.Generator);
  BigNumberPolynomialGaloisPower(Q.X, Q.X, Prime2, Prime, Ecc.Primitive);
  BigNumberPolynomialGaloisPower(Q.Y, Q.Y, Prime2, Prime, Ecc.Primitive); // 算 π2P

  Ecc.PointAddPoint(S, S, Q);
  ShowMessage(Q.ToString);                                          // 得到 0,0

  P.Free;
  Q.Free;
  S.Free;
  G.Free;
  Ecc.Free;
  Prime.Free;
  Prime2.Free;
  PM.Free;
end;

procedure TFormPolynomial.btnBNTestPoly1Click(Sender: TObject);
var
  X, Y, P, E: TCnBigNumberPolynomial;
begin
  X := TCnBigNumberPolynomial.Create([12, 8, 11, 1]);
  Y := TCnBigNumberPolynomial.Create([12, 5, 2, 12]);
  P := TCnBigNumberPolynomial.Create([9, 12, 12, 0, 3]);
  E := TCnBigNumberPolynomial.Create([1, 2, 0, 1]);

  FQ.SetWord(13);
  BigNumberPolynomialGaloisMul(Y, Y, Y, FQ, P); // 计算 PiY 系数的平方
  BigNumberPolynomialGaloisMul(Y, Y, E, FQ, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点 2x3+7x2+12x+5

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入，也得到 2x3+7x2+12x+5
  BigNumberPolynomialGaloisCompose(E, E, X, FQ, P);

  if BigNumberPolynomialEqual(E, Y) then
    ShowMessage('Pi On Curve')
  else
    ShowMessage('Pi NOT');

  E.Free;
  P.Free;
  Y.Free;
  X.Free;
end;

procedure TFormPolynomial.btnBNTestPoly2Click(Sender: TObject);
var
  X, Y, P, E: TCnBigNumberPolynomial;
begin
  X := TCnBigNumberPolynomial.Create([12,11,5,6]);
  Y := TCnBigNumberPolynomial.Create([8,5]);
  P := TCnBigNumberPolynomial.Create([9, 12, 12, 0, 3]);
  E := TCnBigNumberPolynomial.Create([1, 2, 0, 1]);

  FQ.SetWord(13);
  BigNumberPolynomialGaloisMul(Y, Y, Y, FQ, P); // 计算 PiY 系数的平方
  BigNumberPolynomialGaloisMul(Y, Y, E, FQ, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点 2x3+7x2+12x+5

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入，也得到 2x3+7x2+12x+5
  BigNumberPolynomialGaloisCompose(E, E, X, FQ, P);

  if BigNumberPolynomialEqual(E, Y) then
    ShowMessage('Pi^2 On Curve')
  else
    ShowMessage('Pi^2 NOT');

  E.Free;
  P.Free;
  Y.Free;
  X.Free;
end;

procedure TFormPolynomial.btnBNTestPoly3Click(Sender: TObject);
var
  X, Y, P, E: TCnBigNumberPolynomial;
begin
  E := TCnBigNumberPolynomial.Create([1, 2, 0, 1]);
  P := TCnBigNumberPolynomial.Create([7, 6, 1, 9, 10, 11, 12, 12, 9, 3, 7, 5]);

  // 某 Pi
  X := TCnBigNumberPolynomial.Create([9,4,5,6,11,3,8,8,6,2,9]);
  Y := TCnBigNumberPolynomial.Create([12,1,11,0,1,1,7,1,8,9,12,7]);

  FQ.SetWord(13);
  BigNumberPolynomialGaloisMul(Y, Y, Y, FQ, P); // 计算 PiY 系数的平方
  BigNumberPolynomialGaloisMul(Y, Y, E, FQ, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入
  BigNumberPolynomialGaloisCompose(E, E, X, FQ, P);

  if BigNumberPolynomialEqual(E, Y) then
    ShowMessage('Pi On Curve')
  else
    ShowMessage('Pi NOT');

  // 某 Pi^2
  X.SetCoefficents([5,11,3,2,2,7,5,2,11,6,12,5]);
  Y.SetCoefficents([9,3,9,9,2,10,5,3,5,6,2,6]);

  BigNumberPolynomialGaloisMul(Y, Y, Y, FQ, P); // 计算 PiY 系数的平方
  BigNumberPolynomialGaloisMul(Y, Y, E, FQ, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入
  BigNumberPolynomialGaloisCompose(E, E, X, FQ, P);

  if BigNumberPolynomialEqual(E, Y) then
    ShowMessage('Pi^2 On Curve')
  else
    ShowMessage('Pi^2 NOT');

  // 某 3 * P
  X.SetCoefficents([10,8,7,9,5,12,4,12,3,4,1,6]);
  Y.SetCoefficents([7,2,10,0,3,7,4,6,3,0,11,12]);

  BigNumberPolynomialGaloisMul(Y, Y, Y, FQ, P); // 计算 PiY 系数的平方
  BigNumberPolynomialGaloisMul(Y, Y, E, FQ, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入
  BigNumberPolynomialGaloisCompose(E, E, X, FQ, P);

  if BigNumberPolynomialEqual(E, Y) then
    ShowMessage('3 * P On Curve')
  else
    ShowMessage('3 * P NOT');

  // 某点加 π^2 + 3 * P
  X.SetCoefficents([4,5,1,11,4,4,9,6,12,2,6,3]);
  Y.SetCoefficents([2,7,9,11,7,2,9,5,5,6,12,3]);

  BigNumberPolynomialGaloisMul(Y, Y, Y, FQ, P); // 计算 PiY 系数的平方
  BigNumberPolynomialGaloisMul(Y, Y, E, FQ, P); // 再乘以 Y 的平方也就是 X3+AX+B，此时 Y 是椭圆曲线右边的点

  // 再计算 x 坐标，也就是计算 X3+AX+B，把 x 的多项式代入
  BigNumberPolynomialGaloisCompose(E, E, X, FQ, P);

  if BigNumberPolynomialEqual(E, Y) then
    ShowMessage('Pi^2 + 3 * P On Curve')
  else
    ShowMessage('Pi^2 + 3 * P NOT');

  E.Free;
  P.Free;
  Y.Free;
  X.Free;
end;

procedure TFormPolynomial.btnBNTestPointAddClick(Sender: TObject);
var
  X, Y: TCnBigNumberRationalPolynomial;
  A, B, P, Res: TCnBigNumber;
begin
  X := TCnBigNumberRationalPolynomial.Create;
  Y := TCnBigNumberRationalPolynomial.Create;
  Res := TCnBigNumber.Create;
  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  P := TCnBigNumber.Create;

  A.SetOne;
  B.SetOne;
  FQ.SetWord(23);

  X.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.SetOne;

  TCnPolynomialEcc.RationalMultiplePoint(2, X, Y, A, B, FQ);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  if TCnPolynomialEcc.IsRationalPointOnCurve(X, Y, A, B, FQ) then
    ShowMessage('2*P On Curve')
  else
    ShowMessage('2*P NOT On Curve');

  // 验证 6 19 的二倍点是 13 16
  P.SetWord(6);
  BigNumberRationalPolynomialGaloisGetValue(Res, X, P, FQ);
  ShowMessage(Res.ToDec);  // 得到 13 对了
  BigNumberRationalPolynomialGaloisGetValue(Res, Y, P, FQ);
  BigNumberMulWord(Res, 19);
  BigNumberMod(Res, Res, FQ);
  ShowMessage(Res.ToDec);  // 得到 16 对了

  X.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.SetOne;

  TCnPolynomialEcc.RationalMultiplePoint(3, X, Y, A, B, FQ);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  if TCnPolynomialEcc.IsRationalPointOnCurve(X, Y, A, B, FQ) then
    ShowMessage('3*P On Curve')
  else
    ShowMessage('3*P NOT On Curve');

  // 验证 6 19 的三倍点是 7 11
  BigNumberRationalPolynomialGaloisGetValue(Res, X, P, FQ);
  ShowMessage(Res.ToDec);   // 得到 7 对了
  BigNumberRationalPolynomialGaloisGetValue(Res, Y, P, FQ);
  BigNumberMulWord(Res, 19);
  BigNumberMod(Res, Res, FQ);
  ShowMessage(Res.ToDec);   // 得到 11 对了

  X.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.SetOne;

  TCnPolynomialEcc.RationalMultiplePoint(4, X, Y, A, B, FQ);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  if TCnPolynomialEcc.IsRationalPointOnCurve(X, Y, A, B, FQ) then
    ShowMessage('4*P On Curve')
  else
    ShowMessage('4*P NOT On Curve');

  // 验证 6 19 的四倍点是 5 19
  BigNumberRationalPolynomialGaloisGetValue(Res, X, P, FQ);
  ShowMessage(Res.ToDec);   // 得到 5
  BigNumberRationalPolynomialGaloisGetValue(Res, Y, P, FQ);
  BigNumberMulWord(Res, 19);
  BigNumberMod(Res, Res, FQ);
  ShowMessage(Res.ToDec);   // 得到 19

  X.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.SetOne;

  TCnPolynomialEcc.RationalMultiplePoint(5, X, Y, A, B, FQ);
  ShowMessage(X.ToString);
  ShowMessage(Y.ToString);

  if TCnPolynomialEcc.IsRationalPointOnCurve(X, Y, A, B, FQ) then
    ShowMessage('5*P On Curve')
  else
    ShowMessage('5*P NOT On Curve');

  // 验证 6 19 的五倍点是 12 4
  BigNumberRationalPolynomialGaloisGetValue(Res, X, P, FQ);
  ShowMessage(Res.ToDec);  // 得到 12
  BigNumberRationalPolynomialGaloisGetValue(Res, Y, P, FQ);
  BigNumberMulWord(Res, 19);
  BigNumberMod(Res, Res, FQ);
  ShowMessage(Res.ToDec);  // 得到 4

  X.Free;
  Y.Free;
  Res.Free;
end;

procedure TFormPolynomial.btnBNTestManualPointClick(Sender: TObject);
var
  A, B: Int64;
  X, Y: TCnBigNumberRationalPolynomial;
  P, Y2: TCnBigNumberPolynomial;
  RL, RR, T: TCnBigNumberRationalPolynomial;
  Res, V: TCnBigNumber;
begin
  // 简单椭圆曲线二倍点用可除多项式手工计算的结果验证，通过
  X := TCnBigNumberRationalPolynomial.Create;
  Y := TCnBigNumberRationalPolynomial.Create;
  Y2 := TCnBigNumberPolynomial.Create;
  P := TCnBigNumberPolynomial.Create;

  RL := TCnBigNumberRationalPolynomial.Create;
  RR := TCnBigNumberRationalPolynomial.Create;
  T := TCnBigNumberRationalPolynomial.Create;

  A := 1;
  B := 1;
  FQ.SetWord(23);   // 有限域F23上的 Y^2=X^3+X+1  （6，19）* 2 = （13，16）
  Res := TCnBigNumber.Create;
  V := TCnBigNumber.Create;

  // 先求整数域
  X.Nominator.SetCoefficents([A*A, 4-12*B, 4-6*A, 0, 1]);  //  X4 + (4-6A)X2 + (4- 12B)x + A2
  X.Denominator.SetCoefficents([4*B, 4*A, 0, 4]);         //        4X3 + 4AX + 4B

  Y.Nominator.SetCoefficents([-A*A*A-8*B*B, -4*A*B, -5*A*A, 20*B, 5*A, 0, 1]); // X6 + 5AX4 + 20BX3 - 5A2X2 - 4ABX - 8B2 - A3
  Y.Denominator.SetCoefficents([8*B*B, 16*A*B, 8*A*A, 16*B, 16*A, 0, 8]);      //          8(X3+AX+B)(X3+AX+B)

  Y2.SetCoefficents([B, A, 0, 1]);
  // 验证 Y^2 * (x^3+Ax+B) 是否等于 X3 + AX + B

  BigNumberRationalPolynomialMul(Y, Y, Y);
  BigNumberRationalPolynomialMul(Y, Y2, RL); // 得到 Y^2 (x^3+Ax+B)
  RL.Reduce;
  ShowMessage(RL.ToString);

  BigNumberRationalPolynomialMul(X, X, RR);
  BigNumberRationalPolynomialMul(RR, X, RR); // 得到 X^3

  P.SetCoefficents([A]);
  BigNumberRationalPolynomialMul(X, P, T);   // T 得到 A * X
  BigNumberRationalPolynomialAdd(RR, T, RR); // RR 得到 X^3 + AX

  P.SetCoefficents([B]);
  BigNumberRationalPolynomialAdd(RR, P, RR); // RR 得到 X^3 + AX + B
  RR.Reduce;
  ShowMessage(RR.ToString);

  // RL/RR 在整数域内有除式不等，换 Fq 看看，原始点（6，19），二倍点公式套上去得到（13，16）
  X.Nominator.SetCoefficents([A*A, 4-12*B, 4-6*A, 0, 1]);  //  X4 + (4-6A)X2 + (4- 12B)x + A2
  X.Denominator.SetCoefficents([4*B, 4*A, 0, 4]);          //        4X3 + 4AX + 4B
  V.SetWord(6);
  BigNumberRationalPolynomialGaloisGetValue(Res, X, V, FQ);
  ShowMessage('2*X (X=6) using Division Polynomial is ' + Res.ToDec); // 得到 13 对了

  Y.Nominator.SetCoefficents([-A*A*A-8*B*B, -4*A*B, -5*A*A, 20*B, 5*A, 0, 1]); // X6 + 5AX4 + 20BX3 - 5A2X2 - 4ABX - 8B2 - A3
  Y.Denominator.SetCoefficents([8*B*B, 16*A*B, 8*A*A, 16*B, 16*A, 0, 8]);      //          8(X3+AX+B)(X3+AX+B)
  BigNumberRationalPolynomialGaloisGetValue(Res, Y, V, FQ);
  BigNumberMulWord(Res, 19);
  BigNumberMod(Res, Res, FQ);

  ShowMessage('2*Y (X=6) using Division Polynomial is ' + Res.ToDec); // 得到 16 对了

  Y2.SetCoefficents([B, A, 0, 1]);
  // 验证二倍点公式用一倍点坐标算出来的值 Y^2 * (x^3+Ax+B) 是否等于 X3 + AX + B

  BigNumberRationalPolynomialGaloisMul(Y, Y, Y, FQ);
  BigNumberRationalPolynomialGaloisMul(Y, Y2, RL, FQ); // 得到 Y^2 (x^3+Ax+B)
  ShowMessage(RL.ToString);

  BigNumberRationalPolynomialGaloisMul(X, X, RR, FQ);
  BigNumberRationalPolynomialGaloisMul(RR, X, RR, FQ); // 得到 X^3

  P.SetCoefficents([A]);
  BigNumberRationalPolynomialGaloisMul(X, P, T, FQ);   // T 得到 A * X
  BigNumberRationalPolynomialGaloisAdd(RR, T, RR, FQ); // RR 得到 X^3 + AX

  P.SetCoefficents([B]);
  BigNumberRationalPolynomialGaloisAdd(RR, P, RR, FQ); // RR 得到 X^3 + AX + B
  ShowMessage(RR.ToString);

  // RL/RR 在 F23 内表达式还是不等，但各自求值看看，居然相等！
  V.SetWord(6);
  BigNumberRationalPolynomialGaloisGetValue(Res, RL, V, FQ);
  BigNumberRationalPolynomialGaloisGetValue(Res, RR, V, FQ);
  ShowMessage(Res.ToDec);  // 3 = 二倍点 Y 坐标平方 16^2 mod 23 = 3
  ShowMessage(Res.ToDec);  // 3 = 二倍点 X 坐标 13^3 + 13 + 1 mod 23 = 3

  // 再拿另外一个点 （13，16）的二倍点（5，19）试一试，也对
  V.SetWord(13);
  BigNumberRationalPolynomialGaloisGetValue(Res, RL, V, FQ);
  BigNumberRationalPolynomialGaloisGetValue(Res, RR, V, FQ);
  ShowMessage(Res.ToDec);  // 16 = 二倍点 Y 坐标平方 19^2 mod 23 = 16
  ShowMessage(Res.ToDec);  // 16 = 二倍点 X 坐标 5^3 + 5 + 1 mod 23 = 16

  // 如果把 X Y 二倍点公式的模逆多项式求出来，会不会相等？但没有本原多项式，完全没法求逆

  P.Free;
  T.Free;
  RL.Free;
  RR.Free;
  Y2.Free;
  Y.Free;
  X.Free;
  Res.Free;
  V.Free;
end;

procedure TFormPolynomial.btnBNRationalGenerateClick(Sender: TObject);
var
  I, D: Integer;
begin
  D := 4;
  FBRP1.SetZero;
  FBRP2.SetZero;

  Randomize;
  for I := 0 to D do
  begin
    FBRP1.Nominator.Add.SetInteger(Random(16) - 1);
    FBRP2.Nominator.Add.SetInteger(Random(16) - 1);
    FBRP1.Denominator.Add.SetInteger(Random(16) - 1);
    FBRP2.Denominator.Add.SetInteger(Random(16) - 1);
  end;

  edtBNRationalNominator1.Text := FBRP1.Nominator.ToString;
  edtBNRationalNominator2.Text := FBRP2.Nominator.ToString;
  edtBNRationalDenominator1.Text := FBRP1.Denominator.ToString;
  edtBNRationalDenominator2.Text := FBRP2.Denominator.ToString;
end;

procedure TFormPolynomial.btnBNRationalAddClick(Sender: TObject);
begin
  FQ.SetDec(edtBNRationalGalois.Text);
  if chkBNRationalGalois.Checked then
    BigNumberRationalPolynomialGaloisAdd(FBRP1, FBRP2, FBRP3, FQ)
  else
    BigNumberRationalPolynomialAdd(FBRP1, FBRP2, FBRP3);
  edtBNRationalResultNominator.Text := FBRP3.Nominator.ToString;
  edtBNRationalResultDenominator.Text := FBRP3.Denominator.ToString;
end;

procedure TFormPolynomial.btnBNRationalSubClick(Sender: TObject);
begin
  FQ.SetDec(edtBNRationalGalois.Text);
  if chkBNRationalGalois.Checked then
    BigNumberRationalPolynomialGaloisSub(FBRP1, FBRP2, FBRP3, FQ)
  else
    BigNumberRationalPolynomialSub(FBRP1, FBRP2, FBRP3);
  edtBNRationalResultNominator.Text := FBRP3.Nominator.ToString;
  edtBNRationalResultDenominator.Text := FBRP3.Denominator.ToString;
end;

procedure TFormPolynomial.btnBNRationalMulClick(Sender: TObject);
begin
  FQ.SetDec(edtBNRationalGalois.Text);
  if chkBNRationalGalois.Checked then
    BigNumberRationalPolynomialGaloisMul(FBRP1, FBRP2, FBRP3, FQ)
  else
    BigNumberRationalPolynomialMul(FBRP1, FBRP2, FBRP3);
  edtBNRationalResultNominator.Text := FBRP3.Nominator.ToString;
  edtBNRationalResultDenominator.Text := FBRP3.Denominator.ToString;
end;

procedure TFormPolynomial.btnBNRationalDivClick(Sender: TObject);
begin
  FQ.SetDec(edtBNRationalGalois.Text);
  if chkBNRationalGalois.Checked then
    BigNumberRationalPolynomialGaloisDiv(FBRP1, FBRP2, FBRP3, FQ)
  else
    BigNumberRationalPolynomialDiv(FBRP1, FBRP2, FBRP3);
  edtBNRationalResultNominator.Text := FBRP3.Nominator.ToString;
  edtBNRationalResultDenominator.Text := FBRP3.Denominator.ToString;
end;

procedure TFormPolynomial.btnBNTestRationalPointAdd1Click(Sender: TObject);
var
  X, Y, M2X, M2Y, M3X, M3Y, M4X, M4Y, M5X, M5Y: TCnBigNumberRationalPolynomial;
  DP: TCnBigNumberPolynomial;
  A, B: TCnBigNumber;
begin
  // 检查一倍点表达式和二倍点表达式相加，结果是否等于三倍点
  // 一倍点 (x, 1 * y)，二倍点用 RationalMultiplePoint 算

  X := TCnBigNumberRationalPolynomial.Create;
  Y := TCnBigNumberRationalPolynomial.Create;
  M2X := TCnBigNumberRationalPolynomial.Create;
  M2Y := TCnBigNumberRationalPolynomial.Create;
  M3X := TCnBigNumberRationalPolynomial.Create;
  M3Y := TCnBigNumberRationalPolynomial.Create;
  M4X := TCnBigNumberRationalPolynomial.Create;
  M4Y := TCnBigNumberRationalPolynomial.Create;
  M5X := TCnBigNumberRationalPolynomial.Create;
  M5Y := TCnBigNumberRationalPolynomial.Create;
  DP := TCnBigNumberPolynomial.Create;

  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  A.SetWord(6);
  B.SetOne;

  FQ.SetWord(29);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(6, 1, 5, DP, FQ); // 算得 5 阶可除多项式
  ShowMessage('DP5: ' + DP.ToString);

  X.Denominator.SetOne;
  X.Nominator.SetCoefficents([0, 1]);
  Y.Nominator.SetOne;
  Y.Denominator.SetCoefficents([1]);     // ( x/1, 1/1 *y)

  ShowMessage('P2:');
  TCnPolynomialEcc.RationalPointAddPoint(X, Y, X, Y, M2X, M2Y, A, B, FQ, DP);
  ShowMessage(M2X.ToString);  // 应该输出 7x^0,21x^1,17x^2,0x^3,1x^4 / 4x^0,24x^1,0x^2,4x^3
  ShowMessage(M2Y.ToString);  // 应该输出 8x^0,5x^1,23x^2,20x^3,1x^4,0x^5,1x^6 / (8x^0,19x^1,0x^2,8x^3) * y
  // 分母也就是再乘以 y，并将 y^2 替换成 x^3 + 6x + 1，得到 8x^6+9x^4+16x^3+27x^2+9x+8 结果对了

  ShowMessage('P3:');
  TCnPolynomialEcc.RationalPointAddPoint(X, Y, M2X, M2Y, M3X, M3Y, A, B, FQ, DP);
  ShowMessage(M3X.ToString);  // 应该输出 24x^0,8x^1,21x^2,21x^3,18x^4,3x^5,16x^6,27x^7,13x^8,6x^9,1x^10,18x^11 / 27x^0,6x^1,20x^2,19x^3,11x^4,27x^5,2x^6,16x^7,12x^8,2x^9,3x^10,8x^11 结果对了
  ShowMessage(M3Y.ToString);  // 应该输出 18x^0,28x^1,17x^2,16x^3,0x^4,21x^5,10x^6,14x^7,10x^8,12x^9,23x^10,27x^11 / 6x^0,25x^1,25x^2,0x^3,25x^4,9x^5,3x^6,25x^7,6x^8,9x^9,14x^10,1x^11 结果虽然对不上号但经过验算是相等的

  ShowMessage('P4:');
  TCnPolynomialEcc.RationalPointAddPoint(X, Y, M3X, M3Y, M4X, M4Y, A, B, FQ, DP);
  ShowMessage(M4X.ToString);
  ShowMessage(M4Y.ToString);  // 不一致但相等，略

  ShowMessage('P5:');
  TCnPolynomialEcc.RationalPointAddPoint(X, Y, M4X, M4Y, M5X, M5Y, A, B, FQ, DP);
  ShowMessage(M5X.ToString);  // 应该输出 0
  ShowMessage(M5Y.ToString);

  A.Free;
  B.Free;

  DP.Free;
  X.Free;
  Y.Free;
  M2X.Free;
  M2Y.Free;
  M3X.Free;
  M3Y.Free;
  M4X.Free;
  M4Y.Free;
  M5X.Free;
  M5Y.Free;
end;

procedure TFormPolynomial.btnBNTestRationalPointAdd2Click(Sender: TObject);
var
  DP, X, Y, Pi1X, Pi1Y, Pi2X, Pi2Y: TCnBigNumberPolynomial;
  RX, RY: TCnBigNumberRationalPolynomial;
  A, B: TCnBigNumber;
begin
{
  对于 F97 上的椭圆曲线 Y2=X3+31X-12 的五阶扭点，注意系数只要针对 97 同余就相等
  计算 π(x^97, y^97) 与　π(x^97^2, y^97^2) 与 2 * (x, 1*y)

π(x, y) =
[47 x^11 + 11 x^10 - 16 x^9 + 8 x^8 + 44 x^7 + 8 x^6 + 10 x^5 + 12 x^4 - 40 x^3 + 42 x^2 + 11 x + 26,
(6 x^11 + 45 x^10 + 34 x^9 + 28 x^8 - 11 x^7 + 3 x^6 - 3 x^5 + 2 x^4 - 39 x^3 -^48 x^2 - x - 9)y].

π^2(x, y) =
[-17 x^11 + 2 x^10 - 25 x^9 - x^8 + 28 x^7 + 31 x^6 + 25 x^5 - 32 x^4 + 45 x^3 + 26 x^2 + 36 x + 60,
(34 x^11 + 35 x^10 - 8 x^9 - 11 x^8 - 48 x^7 + 34 x^6 - 8 x^5 - 37 x^4 - 21 x^3 + 40 x^2 + 11 x + 48)y].

2 *(x, y) =
[22 x^11 + 17 x^10 + 18 x^9 + 40 x^8 + 41 x^7 - 13 x^6 + 30 x^5 + 11 x^4 - 38 x^3 + 7 x^2 + 20 x + 17,
(-11 x^10 - 17 x^9 - 48 x^8 - 12 x^7 + 17 x^6 + 44 x^5 - 10 x^4 + 8 x^3 + 38 x^2 + 25 x + 24)y].

π^2(x, y) + [2]P =   (就这个不对！如果在 Ring 5 中计算的话，5 阶可除多项式最高 12 次方，所以上述均最高只有 11 次，但和为何冒出了 14 次？)
[-14 x^14 + 15 x^13 - 20 x^12 - 43 x^11 - 10 x^10 - 27 x^9 + 5 x^7 + 11 x^6 + 45 x^5 - 17 x^4 + 30 x^3 - 2 x^2 + 35 x - 46,
(-11 x^14 - 35 x^13 - 26 x^12 - 21 x^11 + 25 x^10 + 23 x^9 + 4 x^8 - 24 x^7 + 9 x^6 + 43 x^5 - 47 x^4 + 26 x^3 + 19 x^2 - 40 x - 32)y].

最后和点的 x 坐标和 π的 1 倍点的 x 坐标有最大公因式 <> 1，y 也一样，所以得到 t5 = 1

  用例来源于一个 PPT

  Counting points on elliptic curves over Fq
           Christiane Peters
        DIAMANT-Summer School on
 Elliptic and Hyperelliptic Curve Cryptography
          September 17, 2008
}

  DP := TCnBigNumberPolynomial.Create;
  Pi1X := TCnBigNumberPolynomial.Create;
  Pi1Y := TCnBigNumberPolynomial.Create;
  Pi2X := TCnBigNumberPolynomial.Create;
  Pi2Y := TCnBigNumberPolynomial.Create;

  X := TCnBigNumberPolynomial.Create;
  Y := TCnBigNumberPolynomial.Create([-12, 31, 0, 1]);
  A := TCnBigNumber.Create;
  A.SetWord(31);
  B := TCnBigNumber.Create;
  B.SetInteger(-12);

  FQ.SetWord(97);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(31, -12, 5, DP, FQ);

  X.MaxDegree := 1;
  X[1].SetOne;                 // x
  BigNumberPolynomialGaloisPower(Pi1X, X, FQ, FQ, DP);
  ShowMessage(Pi1X.ToString);               // 得到正确结果，Ring 几内计算就是 mod f几

  BigNumberPolynomialGaloisPower(Pi1Y, Y, (97 - 1) div 2, FQ, DP);
  ShowMessage(Pi1Y.ToString);               // 得到正确结果，y^q = y^q-1 * y = (x3+Ax+B)^((q-1)/2) * y

  X.MaxDegree := 1;
  X[1].SetOne;                 // x
  BigNumberPolynomialGaloisPower(Pi2X, X, 97 * 97, FQ, DP);
  ShowMessage(Pi2X.ToString);         // 得到基本正确的结果，Ring 几内计算就是 mod f几，原用例最后一项常数项可能有错

  Y.SetCoefficents([-12, 31, 0, 1]);
  BigNumberPolynomialGaloisPower(Pi2Y, Y, (97 * 97 - 1) div 2, FQ, DP);
  ShowMessage(Pi2Y.ToString);               // 得到正确结果，y^q^2 = y^q^2-1 * y = (x3+Ax+B)^((q^2-1)/2) * y

  RX := TCnBigNumberRationalPolynomial.Create;
  RY := TCnBigNumberRationalPolynomial.Create;
  TCnPolynomialEcc.RationalMultiplePoint(2, RX, RY, A, B, FQ);
  // ShowMessage(RX.ToString);
  // ShowMessage(RY.ToString);              // 得到 2P 的 X 和 Y 坐标的有理形式

  BigNumberPolynomialGaloisModularInverse(X, RX.Denominator, DP, FQ);
  BigNumberPolynomialGaloisMul(X, X, RX.Nominator, FQ, DP);
  ShowMessage(X.ToString);               // 用模逆多项式将 2P 的 X 坐标转换为多项式，得到正确结果

  BigNumberPolynomialGaloisModularInverse(Y, RY.Denominator, DP, FQ);
  BigNumberPolynomialGaloisMul(Y, Y, RY.Nominator, FQ, DP);
  ShowMessage(Y.ToString);               // 用模逆多项式将 2P 的 Y 坐标转换为多项式，得到正确结果

  // 不能简单相加，得判断两个 X 是否相等，直接判断模系数等式？
  if BigNumberPolynomialGaloisEqual(Pi2X, X, FQ) then
    ShowMessage('π^2 (x) == 2 * P (x)')
  else
    ShowMessage('π^2 (x) <> 2 * P (x)');

  // 不能简单相加，得判断两个 Y 是否相等，直接判断模系数等式？
  if BigNumberPolynomialGaloisEqual(Pi2Y, Y, FQ) then
    ShowMessage('π^2 (y) == 2 * P (y)')
  else
    ShowMessage('π^2 (y) <> 2 * P (y)');

  RX.Free;
  RY.Free;
  Pi1X.Free;
  Pi1Y.Free;
  Pi2X.Free;
  Pi2Y.Free;
  DP.Free;
  X.Free;
  Y.Free;
end;

procedure TFormPolynomial.btnBNTestGaloisEqualClick(Sender: TObject);
var
  A, B: TCnBigNumberRationalPolynomial;
  DP, TI1, TI2: TCnBigNumberPolynomial;
begin
  A := TCnBigNumberRationalPolynomial.Create;
  B := TCnBigNumberRationalPolynomial.Create;
  DP := TCnBigNumberPolynomial.Create;
  FQ.SetWord(29);
  BigNumberPolynomialGaloisCalcDivisionPolynomial(6, 1, 5, DP, FQ); // 算得 5 阶可除多项式

  // 比较 '6X^11+20X^10+13X^9+20X^8+15X^7+X^6+25X^5+2X^4+13X^3+7X^2+25X+13 / 21X^11+5X^10+12X^9+4X^8+5X^7+23X^6+17X^5+11X^4+22X^3+23X^2+16X+6'
  // 和 27x^11,23x^10,12x^9,10x^8,14x^7,10x^6,21x^5,0x^4,16x^3,17x^2,28x^1,18x^0 / 1x^11,14x^10,9x^9,6x^8,25x^7,3x^6,9x^5,25x^4,0x^3,25x^2,25x^1,6x^0
  A.Nominator.SetCoefficents([13,25,7,13,2,25,1,15,20,13,20,6]);
  A.Denominator.SetCoefficents([6,16,23,22,11,17,23,5,4,12,5,21]);

  B.Nominator.SetCoefficents([18,28,17,16,0,21,10,14,10,12,23,27]);
  B.Denominator.SetCoefficents([6,25,25,0,25,9,3,25,6,9,14,1]);

  TI1 := TCnBigNumberPolynomial.Create;
  TI2 := TCnBigNumberPolynomial.Create;

  BignumberPolynomialGaloisMul(TI1, A.Nominator, B.Denominator, FQ, DP);
  BignumberPolynomialGaloisMul(TI2, A.Denominator, B.Nominator, FQ, DP);

  if BignumberPolynomialGaloisEqual(TI1, TI2, FQ) then
    ShowMessage('Equal')  // 应该得到 Equal
  else
    ShowMessage('NOT Equal');

  TI2.Free;
  TI1.Free;

  B.Free;
  A.Free;
end;

procedure TFormPolynomial.btnInt64PolySetStringClick(Sender: TObject);
begin
  FIP1.SetString(edtIP1.Text);
  ShowMessage(FIP1.ToString);
end;

procedure TFormPolynomial.btnBNPolySetStringClick(Sender: TObject);
begin
  FBP1.SetString(edtBNPolynomial.Text);
  ShowMessage(FBP1.ToString);
end;

procedure TFormPolynomial.btnRationalSetStringClick(Sender: TObject);
begin
  FRP1.SetString(edtRationalNominator1.Text + ' / ' + edtRationalDenominator1.Text);
  ShowMessage(FRP1.ToString);
end;

procedure TFormPolynomial.btnBNRationalSetStringClick(Sender: TObject);
begin
  FBRP1.SetString(edtBNRationalNominator1.Text + ' / ' + edtBNRationalDenominator1.Text);
  ShowMessage(FBRP1.ToString);
end;

procedure TFormPolynomial.btnTestBigDivClick(Sender: TObject);
var
  P: Int64;
begin
  P := 4294967291;
  FQ.SetDec('4294967291');

  FIP1.SetString(A_BIG_POLY);
  if FIP1.ToString <> A_BIG_POLY then
    ShowMessage('IP1 Error');
  FIP2.SetString(B_BIG_POLY);
  if FIP2.ToString <> B_BIG_POLY then
    ShowMessage('IP2 Error');

  FBP1.SetString(A_BIG_POLY);
  if FBP1.ToString <> A_BIG_POLY then
    ShowMessage('BP1 Error');
  FBP2.SetString(B_BIG_POLY);
  if FBP2.ToString <> B_BIG_POLY then
    ShowMessage('BP2 Error');

  Int64PolynomialGaloisDiv(FIP3, nil, FIP1, FIP2, P);
  BigNumberPolynomialGaloisDiv(FBP3, nil, FBP1, FBP2, FQ);

  if FIP3.ToString <> FBP3.ToString then
    ShowMessage('Div Error');
end;

procedure TFormPolynomial.btnTestBigGCDClick(Sender: TObject);
var
  P: Int64;
  IR1, IQ1: TCnInt64Polynomial;
  BR1, BQ1: TCnBigNumberPolynomial;
begin
  P := 4294967291;
  FQ.SetDec('4294967291');

  FIP1.SetString(D1_BIG_POLY);
  if FIP1.ToString <> D1_BIG_POLY then
    ShowMessage('IP1 Error');
  FIP2.SetString(D2_BIG_POLY);
  if FIP2.ToString <> D2_BIG_POLY then
    ShowMessage('IP2 Error');

  FBP1.SetString(D1_BIG_POLY);
  if FBP1.ToString <> D1_BIG_POLY then
    ShowMessage('BP1 Error');
  FBP2.SetString(D2_BIG_POLY);
  if FBP2.ToString <> D2_BIG_POLY then
    ShowMessage('BP2 Error');

  if FIP1.ToString <> FBP1.ToString then
    ShowMessage('IP1 BP1 Error');
  if FIP2.ToString <> FBP2.ToString then
    ShowMessage('IP2 BP2 Error');

  Int64PolynomialGaloisGreatestCommonDivisor(FIP3, FIP1, FIP2, P);
  BigNumberPolynomialGaloisGreatestCommonDivisor(FBP3, FBP1, FBP2, FQ);

  if FIP3.ToString <> FBP3.ToString then  // 第一步出错！
    ShowMessage('GCD Error');

  // 既然 FIP1 是 FIP2 的倍数，那么 GCD 应该是 FIP2 或其系数变化式
  // 既然 FBP1 是 FBP2 的倍数，那么 GCD 应该是 FBP2 或其系数变化式
  if FIP3.ToString <> FIP2.ToString then
    ShowMessage('Int64 GCD String Error');
  if FBP3.ToString <> FBP2.ToString then
    ShowMessage('BigNumber GCD String Error');

  if not Int64PolynomialGaloisEqual(FIP3, FIP2, P) then
    ShowMessage('Int64 GCD Value Error');
  if not BigNumberPolynomialGaloisEqual(FBP3, FBP2, FQ) then
    ShowMessage('BigNumber GCD Value Error');
  // GCD 不是原因式，只能是系数变化式

  ShowMessage(FIP3.ToString);  // GCD 结果确实有部分项不同
  ShowMessage(FBP3.ToString);

  // 发现出错后开始分析
  IR1 := TCnInt64Polynomial.Create;
  IQ1 := TCnInt64Polynomial.Create;
  BR1 := TCnBigNumberPolynomial.Create;
  BQ1 := TCnBigNumberPolynomial.Create;

  // 再验证 GCD 结果 FIP3、FBP3 是否能整除 FIP2、FBP2，
  Int64PolynomialGaloisDiv(FIP1, IR1, FIP2, FIP3, P);
  BigNumberPolynomialGaloisDiv(FBP1, BR1, FBP2, FBP3, FQ);
  ShowMessage(IR1.ToString); // 还有余数，说明 Int64PolynomialGaloisGreatestCommonDivisor 错了
  ShowMessage(BR1.ToString); // 0 是正确的

  // 修正

  IR1.Free;
  IQ1.Free;
  BR1.Free;
  BQ1.Free;
end;

procedure TFormPolynomial.btnInt64ComposeRationalPolynomialClick(
  Sender: TObject);
begin
  if chkRationalPolynomialGalois.Checked then
    Int64RationalPolynomialGaloisCompose(FRP3, FRP1, FIP1, StrToInt(edtRationalPolynomialPrime.Text))
  else
    Int64RationalPolynomialCompose(FRP3, FRP1, FIP1);
  ShowMessage(FRP3.ToString);
end;

procedure TFormPolynomial.btnInt64ComposeRationalRationalClick(
  Sender: TObject);
begin
  if chkRationalPolynomialGalois.Checked then
    Int64RationalPolynomialGaloisCompose(FRP3, FRP1, FRP2, StrToInt(edtRationalPolynomialPrime.Text))
  else
    Int64RationalPolynomialCompose(FRP3, FRP1, FRP2);
  ShowMessage(FRP3.ToString);
end;

procedure TFormPolynomial.btnInt64ComposePolyRationalClick(
  Sender: TObject);
begin
  if chkRationalPolynomialGalois.Checked then
    Int64RationalPolynomialGaloisCompose(FRP3, FIP1, FRP2, StrToInt(edtRationalPolynomialPrime.Text))
  else
    Int64RationalPolynomialCompose(FRP3, FIP1, FRP2);
  ShowMessage(FRP3.ToString);
end;

procedure TFormPolynomial.btnBNRationalRationalClick(Sender: TObject);
begin
  FQ.SetDec(edtBNRationalGalois.Text);
  if chkBNRationalGalois.Checked then
    BigNumberRationalPolynomialGaloisCompose(FBRP3, FBRP1, FBRP2, FQ)
  else
    BigNumberRationalPolynomialCompose(FBRP3, FBRP1, FBRP2);
  ShowMessage(FBRP3.ToString);
end;

procedure TFormPolynomial.btnBNPolyRationalClick(Sender: TObject);
begin
  FQ.SetDec(edtBNRationalGalois.Text);
  if chkBNRationalGalois.Checked then
    BigNumberRationalPolynomialGaloisCompose(FBRP3, FBP1, FBRP2, FQ)
  else
    BigNumberRationalPolynomialCompose(FBRP3, FBP1, FBRP2);
  ShowMessage(FBRP3.ToString);
end;

procedure TFormPolynomial.btnBNRationalPolyClick(Sender: TObject);
begin
  FQ.SetDec(edtBNRationalGalois.Text);
  if chkBNRationalGalois.Checked then
    BigNumberRationalPolynomialGaloisCompose(FBRP3, FBRP1, FBP1, FQ)
  else
    BigNumberRationalPolynomialCompose(FBRP3, FBRP1, FBP1);
  ShowMessage(FBRP3.ToString);
end;

procedure TFormPolynomial.btnCompareRationalMul2MethodClick(
  Sender: TObject);
var
  A, B, Q: Int64;
  DPS: TObjectList;
  Ecc: TCnInt64PolynomialEcc;
  X, Y, X1, Y1, X2, Y2, XT, YT: TCnInt64RationalPolynomial;
  TY2: TCnInt64Polynomial;
  I, J, K: Integer;
begin
  // 比较点加生成的和坐标多项式与直接通过 DivisionPolynomial 计算得到的积坐标多项式是否相等
  // F97 上的椭圆曲线 Y2=X3+31X-12
  Q := 19;
  A := 2;
  B := 1;

  DPS := TObjectList.Create;

  CnInt64GenerateGaloisDivisionPolynomials(A, B, Q, 8, DPS);
  for I := 0 to DPS.Count - 1 do
    mmoEcc.Lines.Add(TCnInt64Polynomial(DPS[I]).ToString);
  mmoEcc.Lines.Add('');

  Ecc := TCnInt64PolynomialEcc.Create(A, B, Q, 2, [0], [0], 0, [0]);

  X := TCnInt64RationalPolynomial.Create;   // X Y 保持不变
  Y := TCnInt64RationalPolynomial.Create;

  X1 := TCnInt64RationalPolynomial.Create;  // 容纳叠加结果
  Y1 := TCnInt64RationalPolynomial.Create;

  X2 := TCnInt64RationalPolynomial.Create;  // 容纳直接计算的结果
  Y2 := TCnInt64RationalPolynomial.Create;

  XT := TCnInt64RationalPolynomial.Create;  // 临时的
  YT := TCnInt64RationalPolynomial.Create;

  for J := 0 to 1 do
  begin
    X.SetOne;
    Y.SetOne;

    if J = 0 then
    begin
      mmoEcc.Lines.Add('For Simple Polynomial:');

      X.Nominator.SetCoefficents([0, 1]);
      Y.Nominator.SetCoefficents([1]);        // (x, 1*y)，在曲线上，下面的多倍点都在曲线上
      K := 4;
    end
    else
    begin
      mmoEcc.Lines.Add('');
      mmoEcc.Lines.Add('For Complex Polynomial:');

      X.Nominator.MaxDegree := Q;             // (x^q, y^q)，是否也在曲线上？
      Y.Nominator.MaxDegree := Q;             // 其中 y^q = (y^2)^(q-1)/2 * y
      X.Nominator[Q] := 1;                    // y^2 可以替换成 x^3+Ax+B

      TY2 := TCnInt64Polynomial.Create([B, A, 0, 1]);
      Int64PolynomialGaloisMul(TY2, TY2, TY2, Q); // 得到 y^2

      Int64PolynomialGaloisPower(Y.Nominator, TY2, (Q - 1) shr 1, Q);
      TY2.Free;

      // 先判断该点在曲线上
      if TCnInt64PolynomialEcc.IsRationalPointOnCurve(X, Y, A, B, Q) then
        ShowMessage('(X^Q, Y^Q) is On Curve.');

      K := 2;
    end;

    X1.SetZero;
    Y1.SetZero;

    for I := 0 to K do
    begin
      TCnInt64PolynomialEcc.RationalPointAddPoint(X1, Y1, X, Y, XT, YT, A, B, Q);
      // X1 和 Y1 是连续相加 X 和 Y 得到的坐标多项式
      Int64RationalPolynomialCopy(X1, XT);
      Int64RationalPolynomialCopy(Y1, YT);

      Int64RationalMultiplePointX(X2, X, I + 1, A, B, Q, DPS);
      Int64RationalMultiplePointY(Y2, X, Y, I + 1, A, B, Q, DPS);
      // X2 和 Y2 是直接通过 DivisionPolynomial 计算得到的积坐标多项式

      mmoEcc.Lines.Add('# ' + IntToStr(I + 1));
      mmoEcc.Lines.Add('  ' + X1.ToString);
      mmoEcc.Lines.Add('  ' + Y1.ToString);
      mmoEcc.Lines.Add('  ' + X2.ToString);
      mmoEcc.Lines.Add('  ' + Y2.ToString);

      if Int64RationalPolynomialGaloisEqual(X1, X2, Q) then
        mmoEcc.Lines.Add('=== X Equal ! ===');
      if Int64RationalPolynomialGaloisEqual(Y1, Y2, Q) then
        mmoEcc.Lines.Add('=== Y Equal ! ===');

      // 再判断该点在曲线上
      if Ecc.IsRationalPointOnCurve(X1, Y1, A, B, Q) then
        mmoEcc.Lines.Add('(X1, Y1) is On Curve.');
    end;
  end;

  XT.Free;
  YT.Free;
  X2.Free;
  Y2.Free;
  X1.Free;
  Y1.Free;
  X.Free;
  Y.Free;
  Ecc.Free;
  DPS.Free;
end;

procedure TFormPolynomial.btnInt64MulDFTClick(Sender: TObject);
begin
  if Int64PolynomialDftMul(FIP3, FIP1, FIP2) then
    edtIP3.Text := FIP3.ToString
  else
    edtIP3.Text := '';
end;

procedure TFormPolynomial.btnTestInt64SimpleDFTClick(Sender: TObject);
begin
  FIP1.SetCoefficents([1, 1, 1]);
  FIP2.SetCoefficents([1, 1, 1]);
  if Int64PolynomialDftMul(FIP3, FIP1, FIP2) then
    ShowMessage(FIP3.ToString);
end;

procedure TFormPolynomial.btnTestInt64SimpleNTTClick(Sender: TObject);
begin
  FIP1.SetCoefficents([1, 1, 1]);
  FIP2.SetCoefficents([1, 1, 1]);
  if Int64PolynomialNttMul(FIP3, FIP1, FIP2) then
    ShowMessage(FIP3.ToString);
end;

procedure TFormPolynomial.btnBiInt64ToStringClick(Sender: TObject);
var
  IBP, IBP1: TCnInt64BiPolynomial;
begin
  IBP := TCnInt64BiPolynomial.Create;

  IBP.SetYCoefficents(0, [23, 4, -45]);
  IBP.SetYCoefficents(1, [0, -78, 23, 34]);
  IBP.SetYCoefficents(3, [1, 0]);
  IBP.SetYCoefficents(4, [-34, 4, -1]);

  edtBIP.Text := IBP.ToString;

  IBP1 := Int64BiPolynomialDuplicate(IBP);
  edtBIP.Text := IBP1.ToString;

  IBP1.Free;
  IBP.Free;
end;

procedure TFormPolynomial.btnIBPAddClick(Sender: TObject);
begin
  if Int64BiPolynomialAdd(FIBP3, FIBP1, FIBP2) then
    edtIBP3.Text := FIBP3.ToString;
end;

procedure TFormPolynomial.btnIBPSubClick(Sender: TObject);
begin
  if Int64BiPolynomialSub(FIBP3, FIBP1, FIBP2) then
    edtIBP3.Text := FIBP3.ToString;
end;

procedure TFormPolynomial.btnIBPMulClick(Sender: TObject);
begin
  if Int64BiPolynomialMul(FIBP3, FIBP1, FIBP2) then
    edtIBP3.Text := FIBP3.ToString;
end;

procedure TFormPolynomial.btnIBP1RandomClick(Sender: TObject);
var
  I, D, X, Y: Integer;
begin
  D := StrToIntDef(edtIBP1Deg.Text, 5);
  FIBP1.SetZero;
  Randomize;

  for I := 0 to D do
  begin
    X := Random(D);
    Y := Random(D);
    FIBP1.SafeValue[X, Y] := Random(32) - 16;
  end;
  mmoIBP1.Lines.Text := FIBP1.ToString;
end;

procedure TFormPolynomial.btnIBP2RandClick(Sender: TObject);
var
  I, D, X, Y: Integer;
begin
  D := StrToIntDef(edtIBP2Deg.Text, 4);
  FIBP2.SetZero;
  Randomize;

  for I := 0 to D do
  begin
    X := Random(D);
    Y := Random(D);
    FIBP2.SafeValue[X, Y] := Random(32) - 16;
  end;
  mmoIBP2.Lines.Text := FIBP2.ToString;
end;

procedure TFormPolynomial.btnBiInt64SetStringClick(Sender: TObject);
begin
  FIBP1.SetString('-X^4Y^2+4X^4Y-34X^4+X^3+34XY^3+23XY^2-78XY-45Y^2+4Y+23');
  edtBIP.Text := FIBP1.ToString;
end;

procedure TFormPolynomial.btnIBPPowerClick(Sender: TObject);
begin
  if Int64BiPolynomialPower(FIBP3, FIBP1, StrToIntDef(edtIBPPower.Text, 2)) then
    edtIBP3.Text := FIBP3.ToString;
end;

procedure TFormPolynomial.btnIBPEvalYClick(Sender: TObject);
var
  S: string;
  Y: Int64;
  Res: TCnInt64Polynomial;
begin
  S := '0';
  if InputQuery('Hint', 'Enter Y Value:', S) then
  begin
    Y := StrToInt64(S);
    Res := TCnInt64Polynomial.Create;
    if Int64BiPolynomialEvaluateByY(Res, FIBP1, Y) then
      edtIBP3.Text := Res.ToString;
    Res.Free;
  end;
end;

procedure TFormPolynomial.btnIBPEvalXClick(Sender: TObject);
var
  S: string;
  X: Int64;
  Res: TCnInt64Polynomial;
begin
  S := '0';
  if InputQuery('Hint', 'Enter X Value:', S) then
  begin
    X := StrToInt64(S);
    Res := TCnInt64Polynomial.Create;
    if Int64BiPolynomialEvaluateByX(Res, FIBP1, X) then
      edtIBP3.Text := Int64PolynomialToString(Res, 'Y');
    Res.Free;
  end;
end;

procedure TFormPolynomial.btnIBPTransposeClick(Sender: TObject);
begin
  Int64BiPolynomialTranspose(FIBP3, FIBP1);
  edtIBP3.Text := FIBP3.ToString;
end;

procedure TFormPolynomial.btnIBPExtractXYClick(Sender: TObject);
var
  P: TCnInt64Polynomial;
  D: Integer;
begin
  D := StrToIntDef(edtIBPExtract.Text, 2);
  P := TCnInt64Polynomial.Create;

  Int64BiPolynomialExtractYByX(P, FIBP1, D);
  mmoIBP2.Lines.Text := Int64PolynomialToString(P, 'Y');
  Int64BiPolynomialExtractXByY(P, FIBP1, D);
  edtIBP3.Text := P.ToString;

  P.Free;
end;

procedure TFormPolynomial.btnIBPIsMonicXClick(Sender: TObject);
begin
  if FIBP1.IsMonicX then
    ShowMessage('Is Monic X')
  else
    ShowMessage('Is NOT Monic X');
end;

procedure TFormPolynomial.btnIPIsMonicClick(Sender: TObject);
begin
  if FIP1.IsMonic then
    ShowMessage('Is Monic')
  else
    ShowMessage('Is NOT Monic');
end;

end.

