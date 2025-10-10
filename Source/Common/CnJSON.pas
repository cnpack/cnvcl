{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnJSON;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�JSON ��������װ��Ԫ�������� DXE6 ������ JSON ������ĳ���
* ��Ԫ���ߣ�CnPack ������
* ��    ע���ʺ� UTF8 ��ע�͸�ʽ������ RFC 7159 ������
*           ע��δ���ϸ�ȫ����ԣ����ʺ���� System.JSON
*           ������ System.JSON �ĵͰ汾�г䵱 JSON ��������װ��
*
*           һ�� JSON ��һ�� JSONObject������һ������ Key Value �ԣ�
*           Key ��˫�����ַ�����Value ���������ֵͨ��JSONObject �� JSONArray��
*           JSONArray ��һ�� JSONValue
*
*           JSONValue ���� Count �� [Integer] ��ȱʡ������Ϊ�� JSONArray ʱ�����
*           ���� Count �� Values[String] ȱʡ������Ϊ�� JSONObject ʱ�����String ����Ϊ Key
*           ��������д Obj['animals']['dog'].Values[0]['age'].AsInteger �ļ�����ʽ������
*
*           ������
*              ���ú��� CnJSONParse������ UTF8 ��ʽ�� JSONString������ JSONObject ����
*
*           ��װ��
*              ���� TCnJSONObject ������� AddPair ����
*              ��Ҫ����ʱ���� TCnJSONArray ������� AddValue ����
*              �� TCnJSONObject ���� ToJSON ���������� UTF8 ��ʽ�� JSON �ַ���
*
*           TCnJSONReader.LoadFromJSON �� TCnJSONWriter.SaveToJSON �ܹ������������������ JSON �ַ�����
*
*           ������ע�⣡����
*           JSON �ڲ�ʹ�� UTF8���������� Name �� Value �� string���� Delphi �и��ݱ�������Ӧ�� Ansi/Utf16
*           �� FPC �� Ansi ģʽ�½������� Name �� Value �� string ���� Ansi��
*           �ⲿ�� UI �򽻵�ʱ����� FPC ��Ҫ���Ծ����Ƿ�ת��Ϊ UTF8 ��ʽ��
*
* ����ƽ̨��PWin7 + Delphi 7
* ���ݲ��ԣ�PWin7 + Delphi 2009 ~
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.08.19 V1.9
*                 ���� FPC ��֧�֡�FPC �� Ansi ģʽ�� string ��ʹ�� Ansi���ݲ�֧�� FPC �� Unicode ģʽ
*           2025.06.14 V1.8
*                 ���ӽ��� [ ��ͷ�� ] ��β���ַ���Ϊ JSONArray �ķ���
*                 ���ӽ�һ�� JSON �����ֵת��Ϊ [ ��ͷ�� ] ��β���ַ����ķ���
*           2025.03.02 V1.7
*                 �� DeepSeek �����������С����
*           2025.03.01 V1.6
*                 ���ӽ��ַ��������ɶ�� JSONObject �Ĺ��̲����Ӳ����������ڽ������������ַ���
*                 ���������ַ���һ��ʼ�ͳ��� \" ʱ�Ĵ������
*           2024.07.30 V1.5
*                 ʵ�� Value �� Clone ����������� AddValues �������ϲ� JSONObject �Ĺ���
*           2024.06.30 V1.4
*                 ���������ַ���ת������ĳЩ���ŵ�ϵͳ�������õ�Ӱ�죬���Ե��С����Ϊ׼
*           2024.02.04 V1.3
*                 JSONObject �� Key Value ������������ֵʱ���ڲ��ù�ϣ����м���
*           2024.02.03 V1.2
*                 ����ȫ�ֺ������޸������������
*           2024.01.11 V1.1
*                 ���뼶��Ĭ�����ԣ����� Reader �� Writer
*           2023.09.15 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, {$IFNDEF COMPILER5} Variants, {$ENDIF} Contnrs, TypInfo,
  SysConst, CnNative, CnStrings, CnHashTable;

type
  ECnJSONException = class(Exception);
  {* JSON ��������쳣}

  TCnJSONTokenType = (jttObjectBegin, jttObjectEnd, jttArrayBegin, jttArrayEnd,
    jttNameValueSep, jttElementSep, jttNumber, jttString, jttNull, jttTrue,
    jttFalse, jttBlank, jttTerminated, jttUnknown);
  {* JSON �еķ������ͣ���Ӧ������š��Ҵ����š��������š��������š��ֺš����š�
    ���֣����������͸��㣩��˫�����ַ�����null��true��false���ո�س���#0��δ֪��}

  TCnJSONParser = class
  {* UTF8 ��ʽ����ע�͵� JSON �ַ���������}
  private
    FRun: Integer;
    FTokenPos: Integer;
    FOrigin: PAnsiChar;
    FStringLen: Integer; // ��ǰ�ַ������ַ�����
    FProcTable: array[#0..#255] of procedure of object;
    FTokenID: TCnJSONTokenType;

    procedure KeywordProc;               // null true false ���ʶ��
    procedure ObjectBeginProc;           // {
    procedure ObjectEndProc;             // }
    procedure ArrayBeginProc;            // [
    procedure ArrayEndProc;              // ]
    procedure NameValueSepProc;          // :
    procedure ArrayElementSepProc;       // ,
    procedure StringProc;                // ˫����
    procedure NumberProc;                // ����
    procedure BlankProc;                 // �ո� Tab �س���
    procedure TerminateProc;             // #0
    procedure UnknownProc;               // δ֪
    function GetToken: AnsiString;
    procedure SetOrigin(const Value: PAnsiChar);
    procedure SetRunPos(const Value: Integer);
    function GetTokenLength: Integer;
  protected
    function TokenEqualStr(Org: PAnsiChar; const Str: AnsiString): Boolean;
    procedure MakeMethodTable;
    procedure StepRun; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    procedure StepBOM;
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure Next;
    {* ������һ�� Token ��ȷ�� TokenID}
    procedure NextNoJunk;
    {* ������һ���� Null �Լ��ǿո� Token ��ȷ�� TokenID}

    property Origin: PAnsiChar read FOrigin write SetOrigin;
    {* �������� UTF8 ��ʽ�� JSON �ַ�������}
    property RunPos: Integer read FRun write SetRunPos;
    {* ��ǰ����λ������� FOrigin ������ƫ��������λΪ�ֽ�����0 ��ʼ}
    property TokenID: TCnJSONTokenType read FTokenID;
    {* ��ǰ Token ����}
    property Token: AnsiString read GetToken;
    {* ��ǰ Token �� UTF8 �ַ������ݲ���������}
    property TokenLength: Integer read GetTokenLength;
    {* ��ǰ Token ���ֽڳ���}
  end;

  TCnJSONString = class;

  TCnJSONPair = class;

  TCnJSONBase = class(TPersistent)
  {* JSON �еĸ�Ԫ�صĻ���}
  private
    FParent: TCnJSONBase;
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; virtual;
    {* ������ JSON ʱ��Ԫ��ƴװ�ã�һ�㲻��Ҫ���û�����}
  public

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; virtual; abstract;
    property Parent: TCnJSONBase read FParent write FParent;
  end;

  TCnJSONValue = class(TCnJSONBase)
  {* ���� JSON �е�ֵ����}
  private
    FContent: AnsiString;
    // ����ʱ�洢 JSON �н������� UTF8 ԭʼ���ݣ���װʱ�� UTF8 �� JSON �ַ�������
    procedure SetContent(const Value: AnsiString);
  protected
    FUpdated: Boolean;

    function GetName(Index: Integer): TCnJSONString; virtual;
    function GetValueByName(const Name: string): TCnJSONValue; virtual;

    function GetCount: Integer; virtual;
    function GetValue(Index: Integer): TCnJSONValue; virtual;
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure Assign(Source: TPersistent); override;
    {* ��ֵ����}

    function Clone: TCnJSONValue; virtual; abstract;
    {* ����һ����������������е����ж���}

    class function FromString(const Value: string): TCnJSONValue;
    {* ��һ�ַ�������ʵ��}
    class function FromInt(const Value: Integer): TCnJSONValue;
    {* ��һ��������ʵ��}
    class function FromFloat(const Value: Extended): TCnJSONValue;
    {* ��һ����������ʵ��}

    // ���·�����װ��
    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;

    // ���·�������ʱ�ж�������
    function IsObject: Boolean; virtual;
    function IsArray: Boolean; virtual;
    function IsString: Boolean; virtual;
    function IsNumber: Boolean; virtual;
    function IsNull: Boolean; virtual;
    function IsTrue: Boolean; virtual;
    function IsFalse: Boolean; virtual;

    // ���·�������ʱȡֵ��
    function AsString: string; virtual;
    function AsInteger: Integer; virtual;
    function AsInt64: Int64; virtual;
    function AsFloat: Extended; virtual;
    function AsBoolean: Boolean; virtual;

    property Content: AnsiString read FContent write SetContent;
    {* ��ֵͨ����ʱ����ԭʼ UTF8 ��ʽ���ַ�������}

    // �� Object �� Array ʱ�ṩ���ģ������ԣ�������ʵ��
    property Names[Index: Integer]: TCnJSONString read GetName;
    {* ���ƶ�������}
    property ValueByName[const Name: string]: TCnJSONValue read GetValueByName; default;
    {* �������ƻ�ȡֵ��ʵ��}

    property Count: Integer read GetCount;
    {* ��������飬��ʾ�������Ԫ������}

    property Values[Index: Integer]: TCnJSONValue read GetValue;
    {* ֵ���������������������������Ԫ�أ�ע��ֵ������ TCnJSONValue �Ĳ�ͬ����ʵ��}
  end;

  TCnJSONArray = class;

{
  object = begin-object [ member *( value-separator member ) ]
           end-object

  member = string name-separator value
}
  TCnJSONObject = class(TCnJSONValue)
  {* ���� JSON �еĶ���ֵ���࣬Ҳ�� JSON �����ࡣ
    ��������ҵ����� Pair���Ӷ���ӹ������������Ķ���ʵ��}
  private
    FPairs: TObjectList;
    FMap: TCnHashTable;
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    {* ���ڲ�����ʱ��� Pair}
    function GetName(Index: Integer): TCnJSONString; override;
    function GetValueByName(const Name: string): TCnJSONValue; override;
    procedure SetValueByName(const Name: string; const Value: TCnJSONValue);
    function GetCount: Integer; override;
    function GetValue(Index: Integer): TCnJSONValue; override;
    procedure SetValue(Index: Integer; const Value: TCnJSONValue);
  public
    constructor Create; override;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure Assign(Source: TPersistent); override;
    {* ��ֵ����}

    function Clone: TCnJSONValue; override;
    {* ����һ�ݶ�����������������е����ж���}

    procedure Clear;
    {* �����������}

    procedure Sort(Recursive: Boolean = True; CompareProc: TListSortCompare = nil);
    {* ����}

    // ���·�����װ��
    function AddPair(const Name: string; Value: TCnJSONValue): TCnJSONPair; overload;
    function AddPair(const Name: string; const Value: string): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Integer): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Int64): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Extended): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Boolean): TCnJSONPair; overload;
    function AddPair(const Name: string): TCnJSONPair; overload;

    function AddArray(const Name: string): TCnJSONArray;
    {* ���һ�������Ŀ����鲢���ظ��������}

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;
    {* ���� UTF8 ��ʽ�� JSON �ַ�����Indent ��ʾ��ʼ����}

    // ���·���������
    function IsObject: Boolean; override;
    {* �����Ƿ��� JSONObject}

    class function FromJSON(const JsonStr: AnsiString): TCnJSONObject;
    {* ���� UTF8 ��ʽ�� JSON �ַ����������¶���}

    procedure GetNames(OutNames: TStrings);
    {* �� Name Value �Ե� Name ���� OutNames �б�}
    property Count: Integer read GetCount;
    {* �ж��ٸ� Name Value ��}

    property Names[Index: Integer]: TCnJSONString read GetName;
    {* ���ƶ�������}
    property Values[Index: Integer]: TCnJSONValue read GetValue write SetValue;
    {* ֵ�����������������ɷ���������Ӧ��ֵ����ע��ֵ������ TCnJSONValue �Ĳ�ͬ����ʵ����
      д�����ɸ����������ò�������ֵ����ԭ��ֵ�����������ͷ�}
    property ValueByName[const Name: string]: TCnJSONValue read GetValueByName write SetValueByName; default;
    {* �������ɸ������ƻ�ȡֵ�����޸������򷵻� nil��
      д�����ɸ����������ò�������ֵ����ԭ��ֵ�����������ͷţ��޸��������׳��쳣}
  end;

  TCnJSONValueClass = class of TCnJSONValue;

{
  string = quotation-mark *char quotation-mark
}
  TCnJSONString = class(TCnJSONValue)
  {* ���� JSON �е��ַ���ֵ���࣬����ʱ Content �ǰ���ǰ��˫�������ڵ�δ����ת�������}
  private
    FValue: string;
    // �� Content ͬ���� string ��ʽ���ݣ�ͬʱ֧�� Unicode �� Ansi
    procedure SetValue(const Value: string);

    function JsonFormatToString(const Str: AnsiString): string;
    {* �� JSON �е����ݽ���ת��󷵻أ�����ȥ���š�����ת���}
    function StringToJsonFormat(const Str: string): AnsiString;
    {* ���ַ�������˫������ת��󷵻�Ϊ JSON ��ʽ���ڲ����� UTF8 ת��
      Str Ϊ string ���ͣ�ͬʱ֧�� Unicode �� Ansi �µ� string}
  public
    class function FromString(const Value: string): TCnJSONString; reintroduce;
    {* ��һ�ַ�������ʵ��}

    function Clone: TCnJSONValue; override;
    {* ����һ����ͬ���ַ�������}

    function IsString: Boolean; override;
    function AsString: string; override;
    {* ���� Content ֵ���� Value �����ء�
       ע���� FPC �� Ansi ģʽ���� UTF8��Delphi ������ Ansi �� Utf16}

    property Value: string read FValue write SetValue;
    {* ��װʱ�����д��ֵ���ڲ�ͬ������ Content
      ͬʱ֧�� Delphi �е� Unicode �� Ansi �µ� string���� FPC �� Utf8 ��ʽ}
  end;

  TCnJSONNumber = class(TCnJSONValue)
  {* ���� JSON �е�����ֵ����}
  private

  public
    class function FromInt(Value: Int64): TCnJSONNumber; reintroduce;
    {* ������ֵ����ʵ��}
    class function FromFloat(Value: Extended): TCnJSONNumber; reintroduce;
    {* �Ӹ�����ֵ����ʵ��}

    function IsNumber: Boolean; override;

    function Clone: TCnJSONValue; override;
    {* ����һ����ͬ�����ֶ���}

    class function FloatToJsonFormat(Value: Extended): AnsiString;
  end;

  TCnJSONNull = class(TCnJSONValue)
  {* ���� JSON �еĿ�ֵ��}
  private

  public
    constructor Create; override;
    function IsNull: Boolean; override;

    function Clone: TCnJSONValue; override;
    {* ����һ�� Null ����}
  end;

  TCnJSONTrue = class(TCnJSONValue)
  {* ���� JSON �е���ֵ����}
  private

  public
    constructor Create; override;
    function IsTrue: Boolean; override;

    function Clone: TCnJSONValue; override;
    {* ����һ�� True ����}
  end;

  TCnJSONFalse = class(TCnJSONValue)
  {* ���� JSON �еļ�ֵ����}
  private

  public
    constructor Create; override;
    function IsFalse: Boolean; override;

    function Clone: TCnJSONValue; override;
    {* ����һ�� False ����}
  end;

{
  array = begin-array [ value *( value-separator value ) ] end-array
}
  TCnJSONArray = class(TCnJSONValue)
  {* ���� JSON �е�������}
  private
    FValues: TObjectList;
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    {* �ڲ���� Value ��Ϊ����Ԫ��}
    function GetCount: Integer; override;
    function GetValue(Index: Integer): TCnJSONValue; override;
  public
    constructor Create; override;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure Assign(Source: TPersistent); override;
    {* ��ֵ����}

    function Clone: TCnJSONValue; override;
    {* ����һ����ͬ��������󣬰������������Ԫ��}

    procedure Clear;
    {* �����������}

    // �ⲿ��װ��
    function AddValue(Value: TCnJSONValue): TCnJSONArray; overload;
    {* ����һֵ�������鲢���У��������鱾��}
    function AddValue(const Value: string): TCnJSONArray; overload;
    {* ����һ�ַ��������飬�������鱾��}
    function AddValue(Value: Int64): TCnJSONArray; overload;
    {* ����һ���������飬�������鱾��}
    function AddValue(Value: Extended): TCnJSONArray; overload;
    {* ����һ�����������飬�������鱾��}
    function AddValue(Value: Boolean): TCnJSONArray; overload;
    {* ����һ Boolean ֵ�����飬�������鱾��}
    function AddValue: TCnJSONArray; overload;
    {* ����һ��ֵ�����飬�������鱾��}

    function AddValues(Values: array of const): TCnJSONArray;
    {* ����һ�����������飬�������鱾������������� TCnJSONValue ʵ���������֮}

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;
    {* ���� UTF8 ��ʽ�� JSON �ַ���}

    property Count: Integer read GetCount;
    {* �������Ԫ������}
    property Values[Index: Integer]: TCnJSONValue read GetValue; default;
    {* �������Ԫ��}
  end;

  TCnJSONPair = class(TCnJSONBase)
  {* ���� JSON �� Object �ڵ� Name �� Value ������࣬������ Name �� Value ����}
  private
    FName: TCnJSONString;
    FValue: TCnJSONValue;
    procedure SetValue(const Value: TCnJSONValue);
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    {* ���� AChild ��Ϊ�� Value}
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure Assign(Source: TPersistent); override;
    {* ��ֵ����}

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;
    {* ���� UTF8 ��ʽ�� JSON �ַ���}

    property Name: TCnJSONString read FName;
    {* �������Զ����������У��������ͷ�}
    property Value: TCnJSONValue read FValue write SetValue;
    {* ֵ�����Զ��������ⲿ���������ã��������ͷ�}
  end;

  TCnJSONReader = class
  {* �� JSON �ж������ĸ��������࣬�ɲ�����ʵ����ֱ�ӵ��� class ������}
  private
    // ����ϵ�У����� False ��ʾ Name �� JSON �в����ڣ�True ���ڣ�Value ����ֵ
    class function ReadStringValue(Obj: TCnJSONObject; const Name: string; out Value: string): Boolean;
    class function ReadIntegerValue(Obj: TCnJSONObject; const Name: string; out Value: Integer): Boolean;
    class function ReadFloatValue(Obj: TCnJSONObject; const Name: string; out Value: Extended): Boolean;
    class function ReadBooleanValue(Obj: TCnJSONObject; const Name: string; out Value: Boolean): Boolean;
    class function ReadInt64Value(Obj: TCnJSONObject; const Name: string; out Value: Int64): Boolean;

    class procedure ReadProperty(Instance: TPersistent; PropInfo: PPropInfo; Obj: TCnJSONObject);
  public
    class procedure Read(Instance: TPersistent; Obj: TCnJSONObject);
    {* ���� Instance �ĸ����������� JSONObject �ж����Ӧ����ֵ}

    class function FileToJSONObject(const FileName: string): TCnJSONObject;
    {* �� JSON �ļ��д��� JSON ����}
    class function FileToJSON(const FileName: string): AnsiString;
    {* �� JSON �ļ������� JSON �ַ���}
    class procedure LoadFromFile(Instance: TPersistent; const FileName: string);
    {* ���� Instance �ĸ����������� JSON �ļ��ж����Ӧ����ֵ}
    class procedure LoadFromJSON(Instance: TPersistent; const JSON: AnsiString);
    {* ���� Instance �ĸ����������� JSON �ַ����ж����Ӧ����ֵ}
  end;

  TCnJSONWriter = class
  {* ������д�� JSON �ĸ��������࣬�ɲ�����ʵ����ֱ�ӵ��� class ������}
  private
    class procedure WriteStringValue(Obj: TCnJSONObject; const Name, Value: string);
    class procedure WriteIntegerValue(Obj: TCnJSONObject; const Name: string; Value: Integer);
    class procedure WriteFloatValue(Obj: TCnJSONObject; const Name: string; Value: Extended);
    class procedure WriteBooleanValue(Obj: TCnJSONObject; const Name: string; Value: Boolean);
    class procedure WriteInt64Value(Obj: TCnJSONObject; const Name: string; Value: Int64);
    class procedure WriteNullValue(Obj: TCnJSONObject; const Name: string);

    class procedure WriteProperty(Instance: TPersistent; PropInfo: PPropInfo; Obj: TCnJSONObject);
  public
    class procedure Write(Instance: TPersistent; Obj: TCnJSONObject);
    {* �� Instance �ĸ����Ը�ֵ�� JSONObject}

    class procedure JSONObjectToFile(Obj: TCnJSONObject; const FileName: string;
       UseFormat: Boolean = True; Indent: Integer = 0; Utf8Bom: Boolean = True);
    {* �� JSON ����д���ļ���UseFormat �����Ƿ��������ʽ��Indent ��ʾ�����ո�����
      Utf8Bom �����Ƿ�д Utf8 �� BOM ͷ}
    class procedure JSONToFile(const JSON: AnsiString; const FileName: string;
      Utf8Bom: Boolean = True);
    {* �� JSON �ַ���д���ļ���Utf8Bom �����Ƿ�д Utf8 �� BOM ͷ}
    class procedure SaveToFile(Instance: TPersistent; const FileName: string;
      Utf8Bom: Boolean = True);
    {* �� Instance �ĸ�����д�� JSON �ļ���Utf8Bom �����Ƿ�д Utf8 �� BOM ͷ}
    class function SaveToJSON(Instance: TPersistent; UseFormat: Boolean = True): AnsiString;
    {* �� Instance �ĸ�����д�� JSON �ַ�����UseFormat �����Ƿ��������ʽ}
  end;

function CnJSONConstruct(Obj: TCnJSONObject; UseFormat: Boolean = True;
  Indent: Integer = 0): AnsiString; overload;
{* ������ JSON ����תΪ UTF8 ��ʽ�� JSON �ַ���}

function CnJSONConstruct(Objects: TObjectList; UseFormat: Boolean = True;
  Indent: Integer = 0): AnsiString; overload;
{* ���б��еĶ�� JSON �����ֵתΪ UTF8 ��ʽ�� JSON �ַ���������� [ ��ͷ ] ��β��
  Objects �б��еĶ�����Ϊ TCnJSONValue ��������}

function CnJSONParse(const JsonStr: AnsiString): TCnJSONObject; overload;
{* ���� UTF8 ��ʽ�� JSON �ַ���Ϊ���� JSON ������Ҫ�ⲿ�ͷ�}

function CnJSONParse(JsonStr: PAnsiChar; Objects: TObjectList): Integer; overload;
{* ���� UTF8 ��ʽ�� JSON �ַ���Ϊ��� JSON ����ÿ��������� Objects �б��У���Ҫ�ⲿ�ͷ�
  ����ֵ��ʾ������������ JSON ����󣬸��ַ��������˶����ֽڡ����ڲ������� JSON �ַ�������������
  �ַ�ָ�� JsonStr + ����ֵ������˵�ַ����±� string(JsonStr)[����ֵ+1]��������һ������������㣬
  Ҳ������һ���ɹ��������Ҵ����ŵĺ�һ����������˵���ǣ�
  �ַ���������Ҵ����Ž�β���ַ�ָ�� JsonStr + ����ֵ��ָ������β�� #0��
  �ַ���β������Ǵ����ż�һЩ�ո��س����У�JsonStr + ����ֵ��ָ���һ���ո���У�
  �ַ���β������ǲ������� JSON �ַ�����JsonStr + ����ֵ��ָ�������Ŀ�ͷ��}

procedure CnJSONMergeObject(FromObj: TCnJSONObject; ToObj: TCnJSONObject;
  Replace: Boolean = False);
{* �� FromObj ��� JSONObject �ļ�ֵ�Ժϲ��� ToObj ��� JSONObject
  ���ֲ����ڵļ�ֵ�Խ����ƺ���룻���ִ��ڵģ�ͬΪ������ֱ��ƴ�ӣ�Ԫ�ز������أ���ͬΪ������ϲ���
  ���������Replace Ϊ False ��ɶ��������Replace Ϊ True�����ƺ��滻}

function CnJSONParseToArray(const JsonStr: AnsiString): TCnJSONArray;
{* ���� UTF8 ��ʽ�� JSON �ַ���Ϊһ�����飬���ڴ��� [ ��ͷ�� ] ��β���ַ�����
  ����ַ������� [ ��ͷ�� ] ��β������ nil}

implementation

{$IFNDEF UNICODE}
uses
  CnWideStrings;
{$ENDIF}

const
  CN_BLANK_CHARSET: set of AnsiChar = [#9, #10, #13, #32]; // RFC �淶��ֻ�����⼸����Ϊ�հ׷�
  CN_INDENT_DELTA = 4; // ���ʱ�������ո�
  CRLF = #13#10;

  CN_NAME_HASH_THRESHOLD = 128;
  {* JSONObject �� Key Value ���������������ֵʱ���ڲ��ù�ϣ����м���}

var
  DummyTermStep: Integer;

resourcestring
  SCnErrorJSONTokenFmt = 'JSON Token %s Expected at Offset %d';
  SCnErrorJSONValueFmt = 'JSON Value Error %s at Offset %d';
  SCnErrorJSONPair = 'JSON Pair Value Conflict';
  SCnErrorJSONTypeMismatch = 'JSON Value Type Mismatch';
  SCnErrorJSONNameNotExistsFmt = 'JSON Name %s NOT Exist.';
  SCnErrorJSONStringParse = 'JSON String Parse Error';
  SCnErrorJSONValueTypeNotImplementedFmt = 'NOT Implemented for this JSON Value Type %s';
  SCnErrorJSONArrayConstsTypeFmt = 'JSON Const Type NOT Support %d';
  SCnErrorJSONArrayTrailingComma = 'JSON Trailing Comma Error in Array';

{$IFDEF SUPPORT_FORMAT_SETTINGS}

var
  JSONFormatSettings: TFormatSettings; // ���� JSON �еĸ�����С����Ϊ . �ţ�����һЩ , �� OS ���Ե���Ӱ��

{$ENDIF}

function JSONDateTimeToStr(Value: TDateTime): string;
begin
  if Trunc(Value) = 0 then
    Result := FormatDateTime('''hh:mm:ss.zzz''', Value)
  else if Frac(Value) = 0 then
    Result := FormatDateTime('''yyyy-mm-dd''', Value)
  else
    Result := FormatDateTime('''yyyy-mm-dd hh:mm:ss.zzz''', Value);
end;

// ע�⣬ÿ�� JSONParseXXXX ����ִ�����P �� TokenID ��ָ�����Ԫ�غ���ڵķǿ�Ԫ��

function JSONParseValue(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONValue; forward;

function JSONParseObject(P: TCnJSONParser; Current: TCnJSONBase; out TermStep: Integer): TCnJSONObject; forward;

procedure JSONCheckToken(P: TCnJSONParser; ExpectedToken: TCnJSONTokenType);
begin
  if P.TokenID <> ExpectedToken then
    raise ECnJSONException.CreateFmt(SCnErrorJSONTokenFmt,
      [GetEnumName(TypeInfo(TCnJSONTokenType), Ord(ExpectedToken)), P.RunPos]);
end;

// �����������ַ���ʱ���ã�Current ���ⲿ�ĸ�����
function JSONParseString(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONString;
begin
  Result := TCnJSONString.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// ��������������ʱ���ã�Current ���ⲿ�ĸ�����
function JSONParseNumber(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONNumber;
begin
  Result := TCnJSONNumber.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// ���������� null ʱ���ã�Current ���ⲿ�ĸ�����
function JSONParseNull(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONNull;
begin
  Result := TCnJSONNull.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// ���������� true ʱ���ã�Current ���ⲿ�ĸ�����
function JSONParseTrue(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONTrue;
begin
  Result := TCnJSONTrue.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// ���������� false ʱ���ã�Current ���ⲿ�ĸ�����
function JSONParseFalse(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONFalse;
begin
  Result := TCnJSONFalse.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// �������������鿪ʼ���� [ ʱ���ã�Current ���ⲿ�ĸ�����
function JSONParseArray(P: TCnJSONParser; Current: TCnJSONBase; out TermStep: Integer): TCnJSONArray;
begin
  Result := TCnJSONArray.Create;
  P.NextNoJunk;

  try
    while not (P.TokenID in [jttTerminated, jttArrayEnd]) do
    begin
      JSONParseValue(P, Result);
      if P.TokenID = jttElementSep then
      begin
        P.NextNoJunk;
        if P.TokenID = jttArrayEnd then // ���һ��Ԫ�غ�������
          raise ECnJSONException.Create(SCnErrorJSONArrayTrailingComma);

        Continue;
      end
      else
        Break;
    end;

    JSONCheckToken(P, jttArrayEnd);
    TermStep := P.RunPos;
  except
    // ������������쳣����Ҫ�ͷ�֮ǰ������ Result ������󣬷������ڴ�й©
    FreeAndNil(Result);

    raise;
  end;

  // ����ٰ���Ч�� Result ����ȥ
  if (Current <> nil) and (Result <> nil) then
    Current.AddChild(Result);

  P.NextNoJunk;
end;

function JSONParseValue(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONValue;
begin
  case P.TokenID of
    jttObjectBegin:
      Result := JSONParseObject(P, Current, DummyTermStep);
    jttString:
      Result := JSONParseString(P, Current);
    jttNumber:
      Result := JSONParseNumber(P, Current);
    jttArrayBegin:
      Result := JSONParseArray(P, Current, DummyTermStep);
    jttNull:
      Result := JSONParseNull(P, Current);
    jttTrue:
      Result := JSONParseTrue(P, Current);
    jttFalse:
      Result := JSONParseFalse(P, Current);
  else
    raise ECnJSONException.CreateFmt(SCnErrorJSONValueFmt,
      [GetEnumName(TypeInfo(TCnJSONTokenType), Ord(P.TokenID)), P.RunPos]);
  end;
end;

// ���������� { ʱ���ã�Ҫ�� Current ���ⲿ������ JSONObject ����� nil
function JSONParseObject(P: TCnJSONParser; Current: TCnJSONBase; out TermStep: Integer): TCnJSONObject;
var
  Pair: TCnJSONPair;
begin
  Result := TCnJSONObject.Create;
  P.NextNoJunk;

  try
    // { ��Ҳ����ֱ��һ�� } ��ʾ�ն���
    while (P.TokenID <> jttTerminated) and (P.TokenID <> jttObjectEnd) do
    begin
      // ����һ�� String
      JSONCheckToken(P, jttString);

      Pair := TCnJSONPair.Create;
      Result.AddChild(Pair);
      Pair.Name.Content := P.Token;            // ���� Pair ���е� Name ������

      // ����һ��ð��
      P.NextNoJunk;
      JSONCheckToken(P, jttNameValueSep);

      P.NextNoJunk;
      JSONParseValue(P, Pair);
      // ����һ�� Value

      if P.TokenID = jttElementSep then        // �ж��ŷָ���˵������һ�� Key Value ��
      begin
        P.NextNoJunk;
        Continue;
      end
      else
        Break;
    end;

    JSONCheckToken(P, jttObjectEnd);
    TermStep := P.RunPos;
  except
    // ������������쳣����Ҫ�ͷ�֮ǰ������ Result ������󣬷������ڴ�й©
    FreeAndNil(Result);

    raise;
  end;

  // ����ٰ���Ч�� Result ����ȥ
  if (Current <> nil) and (Result <> nil) then
    Current.AddChild(Result);
  P.NextNoJunk;
end;

function CnJSONParseToArray(const JsonStr: AnsiString): TCnJSONArray;
var
  P: TCnJSONParser;
begin
  Result := nil;
  P := TCnJSONParser.Create;
  try
    P.SetOrigin(PAnsiChar(JsonStr));

    if P.TokenID in [jttBlank, jttUnknown] then
      P.NextNoJunk;

    if P.TokenID = jttArrayBegin then
      Result := JSONParseArray(P, nil, DummyTermStep);
  finally
    P.Free;
  end;
end;

function CnJSONParse(const JsonStr: AnsiString): TCnJSONObject;
var
  P: TCnJSONParser;
begin
  Result := nil;
  P := TCnJSONParser.Create;
  try
    P.SetOrigin(PAnsiChar(JsonStr));

    while P.TokenID <> jttTerminated do
    begin
      if P.TokenID = jttObjectBegin then
      begin
        Result := JSONParseObject(P, nil, DummyTermStep);
        Exit;
      end;

      P.NextNoJunk;
    end;
  finally
    P.Free;
  end;
end;

function CnJSONParse(JsonStr: PAnsiChar; Objects: TObjectList): Integer;
var
  P: TCnJSONParser;
  Obj: TCnJSONObject;
  Step: Integer;
begin
  Result := 0;
  P := TCnJSONParser.Create;
  try
    P.SetOrigin(JsonStr);
    while P.TokenID <> jttTerminated do
    begin
      // ���ѭ���������˳��㣬һ���������� {��һ�������� { ���������
      if P.TokenID = jttObjectBegin then
      begin
        try
          Obj := JSONParseObject(P, nil, Step);
        except
          Obj := nil;
        end;

        if Obj <> nil then
        begin
          Objects.Add(Obj);
          Result := Step;
        end
        else
          Exit;
      end;

      P.NextNoJunk;
    end;
  finally
    P.Free;
  end;
end;

function CnJSONConstruct(Obj: TCnJSONObject; UseFormat: Boolean;
  Indent: Integer): AnsiString;
begin
  if Obj <> nil then
    Result := Obj.ToJSON(UseFormat, Indent)
  else
    Result := '';
end;

function CnJSONConstruct(Objects: TObjectList; UseFormat: Boolean;
  Indent: Integer): AnsiString;
var
  I: Integer;
begin
  Result := '[';
  if Objects.Count > 0 then
  begin
    for I := 0 to Objects.Count - 1 do
    begin
      if Objects[I] is TCnJSONValue then
      begin
        if I = 0 then
          Result := Result + TCnJSONValue(Objects[I]).ToJSON(UseFormat, Indent)
        else
          Result := Result + ', ' + TCnJSONValue(Objects[I]).ToJSON(UseFormat, Indent);
      end;
    end;
  end;
  Result := Result + ']';
end;

procedure CnJSONMergeObject(FromObj: TCnJSONObject; ToObj: TCnJSONObject;
  Replace: Boolean);
var
  I, J: Integer;
  V, D: TCnJSONValue;
  N: string;
begin
  if (FromObj = nil) or (ToObj = nil) then
    Exit;

  if FromObj.Count = 0 then
    Exit;

  // �����ڵļ�ֵ�Խ����ƺ���룬���ڵģ�������ͽ��и��ǣ���������ƴ�ӣ��������ͺϲ�
  for I := 0 to FromObj.Count - 1 do
  begin
    N := FromObj.Names[I].AsString;
    if N = '' then
      Continue;

    D := ToObj.ValueByName[N];
    V := FromObj.Values[I];
    // N �����ƣ�V �� From ��ֵ��D �� To ��ֵ

    if D = nil then
    begin
      // ��� FromObj ������ Name �� ToObj �ﲻ���ڣ����ƺ���� ToObj
      ToObj.AddPair(N, V.Clone);
    end
    else
    begin
      // ͬ����ֵ���ڣ�����������ƴ�ӣ����Ƕ�����ϲ�
      if (V is TCnJSONArray) and (D is TCnJSONArray) then
      begin
        for J := 0 to TCnJSONArray(V).Count - 1 do
          TCnJSONArray(D).AddValue(TCnJSONArray(V)[J].Clone);
      end
      else if (V is TCnJSONObject) and (D is TCnJSONObject) then
      begin
        CnJSONMergeObject(TCnJSONObject(V), TCnJSONObject(D), Replace);
      end
      else // �����ǣ������滻���������Ͳ���Ҳǿ���滻
      begin
        if Replace then
          ToObj.ValueByName[N] := V.Clone;
      end;
    end;
  end;
end;

{ TCnJSONParser }

procedure TCnJSONParser.ArrayBeginProc;
begin
  StepRun;
  FTokenID := jttArrayBegin;
end;

procedure TCnJSONParser.ArrayElementSepProc;
begin
  StepRun;
  FTokenID := jttElementSep;
end;

procedure TCnJSONParser.ArrayEndProc;
begin
  StepRun;
  FTokenID := jttArrayEnd;
end;

procedure TCnJSONParser.BlankProc;
begin
  repeat
    StepRun;
  until not (FOrigin[FRun] in CN_BLANK_CHARSET);
  FTokenID := jttBlank;
end;

constructor TCnJSONParser.Create;
begin
  inherited Create;
  MakeMethodTable;
end;

destructor TCnJSONParser.Destroy;
begin

  inherited;
end;

function TCnJSONParser.GetToken: AnsiString;
var
  Len: Cardinal;
  OutStr: AnsiString;
begin
  Len := FRun - FTokenPos;                         // ����ƫ����֮���λΪ�ַ���
  SetString(OutStr, (FOrigin + FTokenPos), Len);   // ��ָ���ڴ��ַ�볤�ȹ����ַ���
  Result := OutStr;
end;

function TCnJSONParser.GetTokenLength: Integer;
begin
  Result := FRun - FTokenPos;
end;

procedure TCnJSONParser.KeywordProc;
begin
  FStringLen := 0;
  repeat
    StepRun;
    Inc(FStringLen);
  until not (FOrigin[FRun] in ['a'..'z']); // �ҵ�Сд��ĸ��ϵı�ʶ��β��

  FTokenID := jttUnknown; // ����ô��
  if (FStringLen = 5) and TokenEqualStr(FOrigin + FRun - FStringLen, 'false') then
    FTokenID := jttFalse
  else if FStringLen = 4 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'true') then
      FTokenID := jttTrue
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'null') then
      FTokenID := jttNull;
  end;
end;

procedure TCnJSONParser.MakeMethodTable;
var
  I: AnsiChar;
begin
  for I := #0 to #255 do
  begin
    case I of
      #0:
        FProcTable[I] := TerminateProc;
      #9, #10, #13, #32:
        FProcTable[I] := BlankProc;
      '"':
        FProcTable[I] := StringProc;
      '0'..'9', '+', '-':
        FProcTable[I] := NumberProc;
      '{':
        FProcTable[I] := ObjectBeginProc;
      '}':
        FProcTable[I] := ObjectEndProc;
      '[':
        FProcTable[I] := ArrayBeginProc;
      ']':
        FProcTable[I] := ArrayEndProc;
      ':':
        FProcTable[I] := NameValueSepProc;
      ',':
        FProcTable[I] := ArrayElementSepProc;
      'f', 'n', 't':
        FProcTable[I] := KeywordProc;
    else
      FProcTable[I] := UnknownProc;
    end;
  end;
end;

procedure TCnJSONParser.NameValueSepProc;
begin
  StepRun;
  FTokenID := jttNameValueSep;
end;

procedure TCnJSONParser.Next;
begin
  FTokenPos := FRun;
  FProcTable[FOrigin[FRun]];
end;

procedure TCnJSONParser.NextNoJunk;
begin
  repeat
    Next;
  until not (FTokenID in [jttBlank]);
end;

procedure TCnJSONParser.NumberProc;
begin
  FTokenID := jttNumber;

  // ��ѡ������
  if FOrigin[FRun] in ['-', '+'] then
    StepRun;

  // ��������
  while FOrigin[FRun] in ['0'..'9'] do
    StepRun;

  // ������С������
  if FOrigin[FRun] = '.' then
  begin
    StepRun;
    while FOrigin[FRun] in ['0'..'9'] do
      StepRun;
  end;

  // ָ��
  if FOrigin[FRun] in ['e', 'E'] then
  begin
    StepRun;
    if FOrigin[FRun] in ['-', '+'] then
      StepRun;
    while FOrigin[FRun] in ['0'..'9'] do
      StepRun;
  end;
end;

procedure TCnJSONParser.ObjectBeginProc;
begin
  StepRun;
  FTokenID := jttObjectBegin;
end;

procedure TCnJSONParser.ObjectEndProc;
begin
  StepRun;
  FTokenID := jttObjectEnd;
end;

procedure TCnJSONParser.SetOrigin(const Value: PAnsiChar);
begin
  FOrigin := Value;
  FRun := 0;
  StepBOM;
  Next;
end;

procedure TCnJSONParser.SetRunPos(const Value: Integer);
begin
  FRun := Value;
  Next;
end;

procedure TCnJSONParser.StepBOM;
begin
  if (FOrigin[FRun] <> #239) or (FOrigin[FRun + 1] = #0) then
    Exit;
  if (FOrigin[FRun + 1] <> #187) or (FOrigin[FRun + 2] = #0) then
    Exit;
  if FOrigin[FRun + 2] <> #191 then
    Exit;

  Inc(FRun, 3);
end;

procedure TCnJSONParser.StepRun;
begin
  Inc(FRun);
end;

procedure TCnJSONParser.StringProc;
begin
  StepRun;
  FTokenID := jttString;
  // Ҫ���� UTF8 �ַ�����ҲҪ����ת���ַ��� \ ��� " \ / b f n r t u ֱ������������ " Ϊֹ
  while (FOrigin[FRun] <> '"') and (FOrigin[FRun] <> #0) do
  begin
    if FOrigin[FRun] = '\' then // ��ת���
      StepRun; // ָ��ת��ź���һ���ַ���������ַ���˫���ţ��ᱻ����Խ����������ѭ�������ж�

    StepRun; // Խ��ת��ź�һ�ַ�ָ��ת��ź�ڶ����ַ������������ţ������� u ����ĸ�������ĸ�����������ַ�
  end;

  if FOrigin[FRun] <> #0 then
    StepRun;
end;

procedure TCnJSONParser.TerminateProc;
begin
  FTokenID := jttTerminated;
end;

function TCnJSONParser.TokenEqualStr(Org: PAnsiChar; const Str: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Length(Str) - 1 do
  begin
    if Org[I] <> Str[I + 1] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

procedure TCnJSONParser.UnknownProc;
begin
  StepRun;
  FTokenID := jttUnknown;
end;

{ TCnJSONObject }

function TCnJSONObject.AddArray(const Name: string): TCnJSONArray;
begin
  Result := TCnJSONArray.Create;
  AddPair(Name, Result);
end;

function TCnJSONObject.AddChild(AChild: TCnJSONBase): TCnJSONBase;
var
  I: Integer;
begin
  if AChild is TCnJSONPair then
  begin
    FPairs.Add(AChild);
    AChild.Parent := Self;
    Result := AChild;

    // ���֮ǰ�Ѿ����� THRESHOLD�������� Map��������ζ��� Map
    if (FMap <> nil) or (FPairs.Count > CN_NAME_HASH_THRESHOLD) then
    begin
      if FMap = nil then
      begin
        // ������״γ����򴴽� Map ����֮ǰ�Ķ��ӽ���
        FMap := TCnHashTable.Create;
        for I := 0 to FPairs.Count - 1 do
          FMap.Add(TCnJSONPair(FPairs[I]).Name.AsString, FPairs[I]);
      end;

      FMap.Add(TCnJSONPair(AChild).Name.AsString, Pointer(AChild)); // ��δ����
    end;
  end
  else
    Result := nil;
end;

function TCnJSONObject.AddPair(const Name: string; Value: Integer): TCnJSONPair;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := AnsiString(IntToStr(Value));
  Result := AddPair(Name, V);
end;

function TCnJSONObject.AddPair(const Name, Value: string): TCnJSONPair;
var
  V: TCnJSONString;
begin
  V := TCnJSONString.Create;
  V.Value := Value;
  Result := AddPair(Name, V);
end;

function TCnJSONObject.AddPair(const Name: string; Value: TCnJSONValue): TCnJSONPair;
begin
  Result := TCnJSONPair.Create;
  AddChild(Result);
  Result.Name.Value := Name;
  Result.Value := Value;
end;

function TCnJSONObject.AddPair(const Name: string): TCnJSONPair;
begin
  Result := AddPair(Name, TCnJSONNull.Create);
end;

function TCnJSONObject.AddPair(const Name: string; Value: Boolean): TCnJSONPair;
begin
  if Value then
    Result := AddPair(Name, TCnJSONTrue.Create)
  else
    Result := AddPair(Name, TCnJSONFalse.Create);
end;

function TCnJSONObject.AddPair(const Name: string; Value: Extended): TCnJSONPair;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := TCnJSONNumber.FloatToJsonFormat(Value);
  Result := AddPair(Name, V);
end;

function TCnJSONObject.AddPair(const Name: string; Value: Int64): TCnJSONPair;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := AnsiString(IntToStr(Value));
  Result := AddPair(Name, V);
end;

procedure TCnJSONObject.Assign(Source: TPersistent);
var
  I: Integer;
  JObj: TCnJSONObject;
  Pair: TCnJSONPair;
begin
  if Source is TCnJSONObject then
  begin
    JObj := Source as TCnJSONObject;
    FPairs.Clear;

    for I := 0 to JObj.Count - 1 do
    begin
      Pair := TCnJSONPair.Create;
      Pair.Assign(TCnJSONPair(JObj.FPairs[I]));
      AddChild(Pair);
    end;
  end
  else
    inherited;
end;

procedure TCnJSONObject.Clear;
begin
  FPairs.Clear;
end;

function TCnJSONObject.Clone: TCnJSONValue;
begin
  Result := TCnJSONObject.Create;
  Result.Assign(Self);
end;

constructor TCnJSONObject.Create;
begin
  inherited;
  FPairs := TObjectList.Create(True);
end;

destructor TCnJSONObject.Destroy;
begin
  FMap.Free;
  FPairs.Free;
  inherited;
end;

class function TCnJSONObject.FromJSON(const JsonStr: AnsiString): TCnJSONObject;
begin
  Result := CnJSONParse(JsonStr);
end;

function TCnJSONObject.GetCount: Integer;
begin
  Result := FPairs.Count;
end;

function TCnJSONObject.GetName(Index: Integer): TCnJSONString;
begin
  Result := (FPairs[Index] as TCnJSONPair).Name;
end;

procedure TCnJSONObject.GetNames(OutNames: TStrings);
var
  I: Integer;
begin
  if OutNames <> nil then
  begin
    OutNames.Clear;
    for I := 0 to Count - 1 do
      OutNames.Add((FPairs[I] as TCnJSONPair).Name.AsString);
  end;
end;

function TCnJSONObject.GetValue(Index: Integer): TCnJSONValue;
begin
  Result := (FPairs[Index] as TCnJSONPair).Value;
end;

function TCnJSONObject.GetValueByName(const Name: string): TCnJSONValue;
var
  I: Integer;
  P: Pointer;
begin
  if FMap = nil then
  begin
    for I := 0 to FPairs.Count - 1 do
    begin
      if TCnJSONPair(FPairs[I]).Name.AsString = Name then
      begin
        Result := TCnJSONPair(FPairs[I]).Value;
        Exit;
      end;
    end
  end
  else // ��ɢ��������
  begin
    P := FMap.GetValues(Name);
    if P <> nil then
    begin
      Result := TCnJSONPair(P).Value;
      Exit;
    end;
  end;
  Result := nil;
end;

function TCnJSONObject.IsObject: Boolean;
begin
  Result := True;
end;

procedure TCnJSONObject.SetValue(Index: Integer; const Value: TCnJSONValue);
begin
  (FPairs[Index] as TCnJSONPair).Value := Value;
end;

procedure TCnJSONObject.SetValueByName(const Name: string; const Value: TCnJSONValue);
var
  I: Integer;
  P: Pointer;
begin
  if FMap = nil then
  begin
    for I := 0 to FPairs.Count - 1 do
    begin
      if TCnJSONPair(FPairs[I]).Name.AsString = Name then
      begin
        TCnJSONPair(FPairs[I]).Value := Value;
        Exit;
      end;
    end
  end
  else // ��ɢ��������
  begin
    P := FMap.GetValues(Name);
    if P <> nil then
    begin
      TCnJSONPair(P).Value := Value;
      Exit;
    end;
  end;
  raise ECnJSONException.CreateFmt(SCnErrorJSONNameNotExistsFmt, [Name]);
end;

function ComparePair(Item1, Item2: Pointer): Integer;
var
  P1, P2: TCnJSONPair;
begin
  if (Item1 = nil) and (Item2 = nil) then
    Result := 0
  else if Item1 = nil then
    Result := -1
  else if Item2 = nil then
    Result := 1
  else
  begin
    P1 := TCnJSONPair(Item1);
    P2 := TCnJSONPair(Item2);
    Result := CompareStr(P1.Name.AsString, P2.Name.AsString);
  end;
end;

procedure TCnJSONObject.Sort(Recursive: Boolean;
  CompareProc: TListSortCompare);
var
  I, J: Integer;
  Arr: TCnJSONArray;
begin
  if not Assigned(CompareProc) then
    CompareProc := ComparePair;
  FPairs.Sort(ComparePair);

  if Recursive then // ������ Object Ҳ����
  begin
    for I := 0 to Count - 1 do
    begin
      if Values[I] is TCnJSONObject then
        (Values[I] as TCnJSONObject).Sort(Recursive, CompareProc)
      else if Values[I] is TCnJSONArray then
      begin
        Arr := Values[I] as TCnJSONArray;
        for J := 0 to Arr.Count - 1 do
        begin
          if Arr.Values[J] is TCnJSONObject then
            (Arr.Values[J] as TCnJSONObject).Sort(Recursive, CompareProc);
        end;
      end;
    end;
  end;
end;

function TCnJSONObject.ToJSON(UseFormat: Boolean; Indent: Integer): AnsiString;
var
  I: Integer;
  Bld: TCnStringBuilder;
begin
  if Indent < 0 then
    Indent := 0;

  Bld := TCnStringBuilder.Create(True);
  try
    if UseFormat then
      Bld.Append('{' + CRLF)
    else
      Bld.AppendAnsiChar('{');

    for I := 0 to Count - 1 do
    begin
      if UseFormat then
        Bld.Append(StringOfChar(' ', Indent + CN_INDENT_DELTA));

{$IFDEF UNICODE}
      Bld.AppendAnsi(Names[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
      // Ҫ��ʽ�� Ansi����Ϊ���ݿ����� UTF8�����ܶ������ string ת��
{$ELSE}
      Bld.Append(Names[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
{$ENDIF}

      Bld.AppendAnsiChar(':');
      if UseFormat then
        Bld.AppendAnsiChar(' ');

{$IFDEF UNICODE}
      Bld.AppendAnsi(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
      // Ҫ��ʽ�� Ansi����Ϊ���ݿ����� UTF8�����ܶ������ string ת��
{$ELSE}
      Bld.Append(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
{$ENDIF}

      if I <> Count - 1 then
      begin
        Bld.AppendAnsiChar(',');
        if UseFormat then
          Bld.Append(CRLF);
      end;
    end;

    if UseFormat then
      Bld.Append(CRLF + StringOfChar(' ', Indent) + '}')
    else
      Bld.AppendAnsiChar('}');

    Result := Bld.ToAnsiString;
  finally
    Bld.Free;
  end;
end;

{ TCnJSONValue }

function TCnJSONValue.AsBoolean: Boolean;
begin
  if IsTrue then
    Result := True
  else if IsFalse then
    Result := False
  else
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);
end;

function TCnJSONValue.AsFloat: Extended;
{$IFNDEF SUPPORT_FORMAT_SETTINGS}
var
  E: Integer;
{$ENDIF}
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

{$IFDEF SUPPORT_FORMAT_SETTINGS}
  Result := StrToFloat(string(FContent), JSONFormatSettings);
{$ELSE}
  // D 5 6 û�� TFormatSettings
  Val(string(FContent), Result, E);
  if E <> 0 then
    raise EConvertError.CreateFmt(SInvalidFloat, [FContent]);
{$ENDIF}
end;

function TCnJSONValue.AsInt64: Int64;
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

  Result := StrToInt64(string(FContent));
end;

function TCnJSONValue.AsInteger: Integer;
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

  Result := StrToInt(string(FContent));
end;

procedure TCnJSONValue.Assign(Source: TPersistent);
begin
  if Source is TCnJSONValue then
  begin
    Content := (Source as TCnJSONValue).Content;
  end
  else
    inherited;
end;

function TCnJSONValue.AsString: string;
begin
  Result := string(FContent); // ���෵��ԭʼ����
end;

constructor TCnJSONValue.Create;
begin

end;

destructor TCnJSONValue.Destroy;
begin

  inherited;
end;

class function TCnJSONValue.FromFloat(const Value: Extended): TCnJSONValue;
begin
  Result := TCnJSONNumber.FromFloat(Value);
end;

class function TCnJSONValue.FromInt(const Value: Integer): TCnJSONValue;
begin
  Result := TCnJSONNumber.FromInt(Value);
end;

class function TCnJSONValue.FromString(const Value: string): TCnJSONValue;
begin
  Result := TCnJSONString.FromString(Value);
end;

function TCnJSONValue.GetCount: Integer;
begin
  raise ECnJSONException.CreateFmt(SCnErrorJSONValueTypeNotImplementedFmt, [ClassName]);
end;

function TCnJSONValue.GetName(Index: Integer): TCnJSONString;
begin
  raise ECnJSONException.CreateFmt(SCnErrorJSONValueTypeNotImplementedFmt, [ClassName]);
end;

function TCnJSONValue.GetValue(Index: Integer): TCnJSONValue;
begin
  raise ECnJSONException.CreateFmt(SCnErrorJSONValueTypeNotImplementedFmt, [ClassName]);
end;

function TCnJSONValue.GetValueByName(const Name: string): TCnJSONValue;
begin
  raise ECnJSONException.CreateFmt(SCnErrorJSONValueTypeNotImplementedFmt, [ClassName]);
end;

function TCnJSONValue.IsArray: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsFalse: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsNull: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsNumber: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsObject: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsString: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsTrue: Boolean;
begin
  Result := False;
end;

procedure TCnJSONValue.SetContent(const Value: AnsiString);
begin
  FContent := Value;
  FUpdated := True;
end;

function TCnJSONValue.ToJSON(UseFormat: Boolean; Indent: Integer): AnsiString;
begin
  // FContent �� UTF8 ��ʽ
  Result := FContent;
end;

{ TCnJSONArray }

function TCnJSONArray.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  if AChild is TCnJSONValue then
  begin
    FValues.Add(AChild);
    AChild.Parent := Self;
    Result := AChild;
  end
  else
    Result := nil;
end;

function TCnJSONArray.AddValue(const Value: string): TCnJSONArray;
var
  V: TCnJSONString;
begin
  V := TCnJSONString.Create;
  V.Value := Value;
  Result := AddValue(V);
end;

function TCnJSONArray.AddValue(Value: TCnJSONValue): TCnJSONArray;
begin
  if Value <> nil then
    FValues.Add(Value);
  Result := Self;
end;

function TCnJSONArray.AddValue(Value: Int64): TCnJSONArray;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := AnsiString(IntToStr(Value));
  Result := AddValue(V);
end;

function TCnJSONArray.AddValue(Value: Boolean): TCnJSONArray;
begin
  if Value then
    Result := AddValue(TCnJSONTrue.Create)
  else
    Result := AddValue(TCnJSONFalse.Create)
end;

function TCnJSONArray.AddValue(Value: Extended): TCnJSONArray;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := TCnJSONNumber.FloatToJsonFormat(Value);
  Result := AddValue(V);
end;

function TCnJSONArray.AddValue: TCnJSONArray;
begin
  Result := AddValue(TCnJSONNull.Create);
end;

function TCnJSONArray.AddValues(Values: array of const): TCnJSONArray;
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
  begin
    case Values[I].VType of
      vtInteger:
        begin
          AddValue(Values[I].VInteger);
        end;
      vtInt64:
        begin
          AddValue(Values[I].VInt64^);
        end;
      vtExtended:
        begin
          AddValue(Values[I].VExtended^);
        end;
      vtBoolean:
        begin
          AddValue(Values[I].VBoolean);
        end;
      vtObject:
        begin
          if Values[I].VObject = nil then
            AddValue
          else if Values[I].VObject is TCnJSONValue then
            AddValue(Values[I].VObject as TCnJSONValue);
        end;
      vtPointer:
        begin
          if Values[I].VPointer = nil then // ָ������ֻ֧�� nil Ϊ null
            AddValue
          else
            raise ECnJSONException.CreateFmt(SCnErrorJSONArrayConstsTypeFmt, [Values[I].VType]);
        end;
      vtString:
        begin
          AddValue(string(Values[I].VString^));
        end;
      vtAnsiString:
        begin
          AddValue(string(PAnsiChar(Values[I].VAnsiString)));
        end;
      vtWideString:
        begin
          AddValue(string(PWideChar(Values[I].VWideString)));
        end;
      vtChar:
        begin
          AddValue(string(Values[I].VChar));  // ע�ⲻ��������仯��ֻ�� AnsiChar
        end;
      vtWideChar:
        begin
          AddValue(string(Values[I].VWideChar));
        end;
      vtPChar:
        begin
          AddValue(string(Values[I].VPChar)); // ע�ⲻ��������仯��ֻ�� PAnsiChar
        end;
      vtPWideChar:
        begin
          AddValue(string(Values[I].VPWideChar));
        end;
{$IFDEF UNICODE}
      vtUnicodeString:
        begin
          AddValue(string(Values[I].VUnicodeString));
        end;
{$ENDIF}
    else
      raise ECnJSONException.CreateFmt(SCnErrorJSONArrayConstsTypeFmt, [Values[I].VType]);
    end;
  end;
  Result := Self;
end;

procedure TCnJSONArray.Assign(Source: TPersistent);
var
  I: Integer;
  Clz: TCnJSONValueClass;
  V: TCnJSONValue;
  Arr: TCnJSONArray;
begin
  if Source is TCnJSONArray then
  begin
    Arr := Source as TCnJSONArray;

    FValues.Clear;
    for I := 0 to Arr.Count - 1 do
    begin
      Clz := TCnJSONValueClass(Arr.Values[I].ClassType);
      V := TCnJSONValue(Clz.NewInstance);
      V.Create;
      V.Assign(Arr.Values[I]);

      AddValue(V);
    end;
  end
  else
    inherited;
end;

procedure TCnJSONArray.Clear;
begin
  FValues.Clear;
end;

function TCnJSONArray.Clone: TCnJSONValue;
begin
  Result := TCnJSONArray.Create;
  Result.Assign(Self);
end;

constructor TCnJSONArray.Create;
begin
  inherited;
  FValues := TObjectList.Create(True);
end;

destructor TCnJSONArray.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TCnJSONArray.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TCnJSONArray.GetValue(Index: Integer): TCnJSONValue;
begin
  Result := TCnJSONValue(FValues[Index]);
end;

function TCnJSONArray.ToJSON(UseFormat: Boolean; Indent: Integer): AnsiString;
var
  Bld: TCnStringBuilder;
  I: Integer;
begin
  Bld := TCnStringBuilder.Create(True);
  try
    Bld.AppendAnsiChar('[');
    if UseFormat then
      Bld.Append(CRLF + StringOfChar(' ', Indent + CN_INDENT_DELTA));

    for I := 0 to Count - 1 do
    begin
{$IFDEF UNICODE}
      Bld.AppendAnsi(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
{$ELSE}
      Bld.Append(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
{$ENDIF}
      if I <> Count - 1 then
      begin
        Bld.AppendAnsiChar(',');
        if UseFormat then
          Bld.AppendAnsiChar(' ');
      end;
    end;

    if UseFormat then
    begin
      Bld.Append(CRLF);
      Bld.Append(StringOfChar(' ', Indent) + ']');
    end
    else
      Bld.AppendAnsiChar(']');

    Result := Bld.ToAnsiString;
  finally
    Bld.Free;
  end;
end;

{ TCnJSONPair }

function TCnJSONPair.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  if FValue <> nil then
    raise ECnJSONException.Create(SCnErrorJSONPair);

  if AChild is TCnJSONValue then
  begin
    FValue := AChild as TCnJSONValue;
    AChild.Parent := Self;
    Result := AChild;
  end
  else
    Result := nil;
end;

procedure TCnJSONPair.Assign(Source: TPersistent);
var
  Clz: TCnJSONValueClass;
  Pair: TCnJSONPair;
begin
  if Source is TCnJSONPair then
  begin
    Pair := Source as TCnJSONPair;
    FName.Assign(Pair.Name);

    if Pair.Value <> nil then
    begin
      Clz := TCnJSONValueClass(Pair.Value.ClassType);
      FValue := TCnJSONValue(Clz.NewInstance);
      FValue.Create;
      FValue.Assign(Pair.Value);
    end;
  end
  else
    inherited;
end;

constructor TCnJSONPair.Create;
begin
  inherited;
  FName := TCnJSONString.Create;
  // FValue ���Ͳ�һ�����ȴ���
end;

destructor TCnJSONPair.Destroy;
begin
  FValue.Free;
  FName.Free;
  inherited;
end;

procedure TCnJSONPair.SetValue(const Value: TCnJSONValue);
begin
  if FValue <> nil then // ����Ѿ����� FValue ���ͷŵ�
    FreeAndNil(FValue);

  FValue := Value;
end;

function TCnJSONPair.ToJSON(UseFormat: Boolean; Indent: Integer): AnsiString;
begin
  // ��������Ӧ���õ����
  Result := '';
end;

{ TCnJSONBase }

function TCnJSONBase.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  Result := AChild;
  AChild.Parent := Self;
end;

{ TCnJSONString }

function TCnJSONString.AsString: string;
begin
  if FUpdated then
  begin
    FValue := JsonFormatToString(Content);
    FUpdated := False;
  end;
  Result := FValue;
end;

function TCnJSONString.Clone: TCnJSONValue;
begin
  Result := TCnJSONString.Create;
  Result.Assign(Self);
end;

class function TCnJSONString.FromString(const Value: string): TCnJSONString;
begin
  Result := TCnJSONString.Create;
  Result.SetValue(Value);
end;

function TCnJSONString.IsString: Boolean;
begin
  Result := True;
end;

function TCnJSONString.JsonFormatToString(const Str: AnsiString): string;
var
  Bld: TCnStringBuilder;
  P: PWideChar;
  U: Integer;
{$IFDEF UNICODE}
  WS: string;
{$ELSE}
  WS: WideString;
{$ENDIF}
  B0, B1, B2, B3: Byte;

  procedure CheckHex(B: Byte);
  begin
    if not (AnsiChar(B) in ['0'..'9', 'A'..'F', 'a'..'f']) then
      raise ECnJSONException.Create(SCnErrorJSONStringParse);
  end;

  function HexToDec(const Value: Byte): Integer;
  begin
    if Value > Ord('9') then
    begin
      if Value > Ord('F') then
        Result := Value - Ord('a') + 10
      else
        Result := Value - Ord('A') + 10;
    end
    else
      Result := Value - Ord('0');
  end;

begin
  Result := '';
  if Length(Str) = 0 then
    Exit;

  // Unicode ������ʹ��ϵͳ��ת��������ʹ�� CnWideStrings ���ת��
{$IFDEF UNICODE}
  WS := UTF8ToUnicodeString(Str);
{$ELSE}
  WS := CnUtf8DecodeToWideString(Str);
{$ENDIF}

  if Length(WS) = 0 then
    raise ECnJSONException.Create(SCnErrorJSONStringParse); // UTF8 ����ʧ��

  P := @WS[1];
  if P^ <> '"' then
    raise ECnJSONException.Create(SCnErrorJSONStringParse);

  Bld := TCnStringBuilder.Create(False);  // ����˫�ֽ��ַ������� Wide ģʽ
  try
    Inc(P);
    while (P^ <> '"') and (P^ <> #0) do
    begin
      if P^ = '\' then
      begin
        Inc(P);
        case P^ of
          '\': Bld.AppendWideChar('\');
          '/': Bld.AppendWideChar('/');
          '"': Bld.AppendWideChar('"');
          'b': Bld.AppendWideChar(#$08);
          't': Bld.AppendWideChar(#$09);
          'n': Bld.AppendWideChar(#$0A);
          'f': Bld.AppendWideChar(#$0C);
          'r': Bld.AppendWideChar(#$0D);
          'u': // u �����ĸ�ʮ�������ַ��� FFFF����֧�ָ�����
            begin
              Inc(P);
              B3 := Ord(P^);
              CheckHex(B3);

              Inc(P);
              B2 := Ord(P^);
              CheckHex(B2);

              Inc(P);
              B1 := Ord(P^);
              CheckHex(B1);

              Inc(P);
              B0 := Ord(P^);
              CheckHex(B0);

              U := (HexToDec(B3) shl 12) or (HexToDec(B2) shl 8) or (HexToDec(B1) shl 4) or HexToDec(B0);
              Bld.AppendWideChar(WideChar(U));
            end;
        else
          raise ECnJSONException.Create(SCnErrorJSONStringParse);
        end;
      end
      else
        Bld.AppendWideChar(P^);
      Inc(P);
    end;

{$IFDEF UNICODE}
    Result := Bld.ToString;
    // Unicode �汾��ʹ�� Wide �汾��ֱ����� string
{$ELSE}
    Result := AnsiString(Bld.ToWideString);
    // �� Unicode ��ǿ��ʹ�� Wide �汾ʱֻ֧����� WideString������ǿ��ת���� AnsiString
{$ENDIF}
  finally
    Bld.Free;
  end;
end;

procedure TCnJSONString.SetValue(const Value: string);
begin
  FValue := Value;
  Content := StringToJsonFormat(Value);
  FUpdated := False; // �� Value ����Ķ� Content �ĸ��£�Content ��������ȥ���� FValue
end;

function TCnJSONString.StringToJsonFormat(const Str: string): AnsiString;
var
  Bld: TCnStringBuilder;
  P: PChar;
begin
  // �������Լ�ת������� UTF8 ת��
  Bld := TCnStringBuilder.Create; // Delphi �и����Ƿ� Unicode ������ Wide �� Ansi��FPC ���� Ansi
  try
    Bld.AppendChar('"');
    if Length(Str) > 0 then
    begin
      P := @Str[1];
      while P^ <> #0 do
      begin
        case P^ of // ע������ʱ������ / ��ת��
          '\':
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('\');
            end;
          '"':
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('"');
            end;
          #$08:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('b');
            end;
          #$09:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('t');
            end;
          #$0A:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('n');
            end;
          #$0C:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('f');
            end;
          #$0D:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('r');
            end;
        else
          Bld.AppendChar(P^);
        end;
        Inc(P);
      end;
    end;

    Bld.AppendChar('"');

{$IFDEF UNICODE}
    Result := UTF8Encode(Bld.ToString);
    // Unicode ������ StringBuilder �ڲ�ʹ�� Wide ģʽ������ UnicodeString��ֱ�ӱ���� UTF8
{$ELSE}
    // �� Unicode ������ StringBuilder �ڲ�ʹ�� Ansi ģʽ������ AnsiString���ڲ�������˫�ֽ��ַ���
    // ת�� WideString ������ UTF8
    Result := CnUtf8EncodeWideString(WideString(Bld.ToString));
{$ENDIF}
  finally
    Bld.Free;
  end;
end;

{ TCnJSONNumber }

function TCnJSONNumber.Clone: TCnJSONValue;
begin
  Result := TCnJSONNumber.Create;
  Result.Assign(Self);
end;

class function TCnJSONNumber.FloatToJsonFormat(Value: Extended): AnsiString;
begin
{$IFDEF SUPPORT_FORMAT_SETTINGS}
  Result := AnsiString(FloatToStr(Value, JSONFormatSettings));
{$ELSE}
  Result := AnsiString(FloatToStr(Value));
  // D 5 6 �²�֧�� TFormatSettings��������Ϊ�������ý�С���������˶��ţ��滻����
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
  // TODO: ���������ĳЩ��������ʹ FloatToStr ���ֶ���С����Ͷ���ǧλ�ָ�����
{$ENDIF}
end;

class function TCnJSONNumber.FromFloat(Value: Extended): TCnJSONNumber;
begin
  Result := TCnJSONNumber.Create;
  Result.SetContent(FloatToStr(Value));
end;

class function TCnJSONNumber.FromInt(Value: Int64): TCnJSONNumber;
begin
  Result := TCnJSONNumber.Create;
  Result.SetContent(IntToStr(Value));
end;

function TCnJSONNumber.IsNumber: Boolean;
begin
  Result := True;
end;

{ TCnJSONNull }

function TCnJSONNull.Clone: TCnJSONValue;
begin
  Result := TCnJSONNull.Create;
end;

constructor TCnJSONNull.Create;
begin
  inherited;
  FContent := 'null';
end;

function TCnJSONNull.IsNull: Boolean;
begin
  Result := True;
end;

{ TCnJSONTrue }

function TCnJSONTrue.Clone: TCnJSONValue;
begin
  Result := TCnJSONTrue.Create;
end;

constructor TCnJSONTrue.Create;
begin
  inherited;
  FContent := 'true';
end;

function TCnJSONTrue.IsTrue: Boolean;
begin
  Result := True;
end;

{ TCnJSONFalse }

function TCnJSONFalse.Clone: TCnJSONValue;
begin
  Result := TCnJSONFalse.Create;
end;

constructor TCnJSONFalse.Create;
begin
  inherited;
  FContent := 'false';
end;

function TCnJSONFalse.IsFalse: Boolean;
begin
  Result := True;
end;

{ TCnJSONReader }

class function TCnJSONReader.FileToJSON(const FileName: string): AnsiString;
var
  F: TFileStream;
begin
  // ���� UTF8Bom ��ԭʼ���룬����ֱ�Ӷ��룬������ UTF16 ��ʽҲ������ UTF 16 �����
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if F.Size > 0 then
    begin
      SetLength(Result, F.Size);
      F.Read(Result[1], F.Size);

      // ȥ�� UTF8 �� BOM ͷ
      if Length(Result) > SizeOf(SCN_BOM_UTF8) then
      begin
        if CompareMem(@Result[1], @SCN_BOM_UTF8[0], SizeOf(SCN_BOM_UTF8)) then
          Delete(Result, 1, SizeOf(SCN_BOM_UTF8));
      end;
    end;
  finally
    F.Free;
  end;
end;

class function TCnJSONReader.FileToJSONObject(const FileName: string): TCnJSONObject;
var
  JSON: AnsiString;
begin
  JSON := FileToJSON(FileName);
  Result := CnJSONParse(JSON);
end;

class procedure TCnJSONReader.LoadFromFile(Instance: TPersistent;
  const FileName: string);
var
  S: AnsiString;
begin
  S := FileToJSON(FileName);
  LoadFromJSON(Instance, S);
end;

class procedure TCnJSONReader.LoadFromJSON(Instance: TPersistent;
  const JSON: AnsiString);
var
  Obj: TCnJSONObject;
  Reader: TCnJSONReader;
begin
  Obj := nil;
  Reader := nil;

  try
    Obj := CnJSONParse(JSON);
    Reader := TCnJSONReader.Create;

    Reader.Read(Instance, Obj)
  finally
    Reader.Free;
    Obj.Free;
  end;
end;

class procedure TCnJSONReader.Read(Instance: TPersistent; Obj: TCnJSONObject);
var
  PropCount: Integer;
  PropList: PPropList;
  I: Integer;
  PropInfo: PPropInfo;
  Value: TCnJSONValue;
  Arr: TCnJSONArray;
begin
  PropCount := GetTypeData(Instance.ClassInfo)^.PropCount;
  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(Pointer));
    try
      GetPropInfos(Instance.ClassInfo, PropList);
      for I := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[I];
        if PropInfo = nil then
          Break;

        ReadProperty(Instance, PropInfo, Obj);
      end;
    finally
      FreeMem(PropList, PropCount * SizeOf(Pointer));
    end;
  end;

  if Instance is TCollection then
  begin
    Value := Obj.ValueByName['Items'];
    if (Value <> nil) and (Value is TCnJSONArray) then
    begin
      Arr := Value as TCnJSONArray;
      (Instance as TCollection).Clear;

      for I := 0 to Arr.Count - 1 do
      begin
        Value := Arr.Values[I];
        if Value is TCnJSONObject then
          Read((Instance as TCollection).Add, Value as TCnJSONObject);
      end;
    end;
  end;
end;

class function TCnJSONReader.ReadBooleanValue(Obj: TCnJSONObject;
  const Name: string; out Value: Boolean): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONFalse then
    begin
      Value := False;
      Result := True;
    end
    else if V is TCnJSONTrue then
    begin
      Value := True;
      Result := True;
    end;
  end;
end;

class function TCnJSONReader.ReadFloatValue(Obj: TCnJSONObject;
  const Name: string; out Value: Extended): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONNumber then
    begin
      Value := V.AsFloat;
      Result := True;
    end;
  end;
end;

class function TCnJSONReader.ReadInt64Value(Obj: TCnJSONObject;
  const Name: string; out Value: Int64): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONNumber then
    begin
      Value := V.AsInt64;
      Result := True;
    end;
  end;
end;

class function TCnJSONReader.ReadIntegerValue(Obj: TCnJSONObject;
  const Name: string; out Value: Integer): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONNumber then
    begin
      Value := V.AsInteger;
      Result := True;
    end;
  end;
end;

class function TCnJSONReader.ReadStringValue(Obj: TCnJSONObject;
  const Name: string; out Value: string): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONString then
    begin
      Value := V.AsString;
      Result := True;
    end;
  end;
end;

class procedure TCnJSONReader.ReadProperty(Instance: TPersistent;
  PropInfo: PPropInfo; Obj: TCnJSONObject);
var
  PropType: PTypeInfo;

{$IFDEF FPC}

  procedure ReadBoolProp;
  var
    Value: Boolean;
  begin
    if ReadBooleanValue(Obj, string(PropInfo^.Name), Value) then
      SetOrdProp(Instance, PropInfo, Ord(Value));
  end;

{$ENDIF}

  procedure ReadStrProp;
  var
    Value: string;
  begin
    if ReadStringValue(Obj, string(PropInfo^.Name), Value) then
      SetStrProp(Instance, PropInfo, Value);
  end;

  procedure ReadInt64Prop;
  var
    Value: Int64;
  begin
    if ReadInt64Value(Obj, string(PropInfo^.Name), Value) then
      SetInt64Prop(Instance, PropInfo, Value);
  end;

  procedure ReadFloatProp;
  var
    Value: Extended;
  begin
    if ReadFloatValue(Obj, string(PropInfo^.Name), Value) then
      SetFloatProp(Instance, PropInfo, Value);
  end;

  procedure ReadOrdProp;
  var
    VI: Integer;
    VS: string;
    VB: Boolean;
  begin
    case PropType^.Kind of
      tkInteger:
        begin
          if ReadIntegerValue(Obj, string(PropInfo^.Name), VI) then
            SetOrdProp(Instance, string(PropInfo^.Name), VI);
        end;
      tkChar:
        begin
          if ReadStringValue(Obj, string(PropInfo^.Name), VS) then
            if Length(VS) > 0 then
              SetOrdProp(Instance, string(PropInfo^.Name), Ord(VS[1]));
        end;
      tkSet:
        begin
          if ReadStringValue(Obj, string(PropInfo^.Name), VS) then
            SetSetProp(Instance, string(PropInfo^.Name), VS);
        end;
      tkEnumeration:
        begin
          if PropType = TypeInfo(Boolean) then
          begin
            if ReadBooleanValue(Obj, string(PropInfo^.Name), VB) then
              SetOrdProp(Instance, string(PropInfo^.Name), Ord(VB));
          end
          else
          begin
            if ReadStringValue(Obj, string(PropInfo^.Name), VS) then
              SetEnumProp(Instance, string(PropInfo^.Name), VS);
          end;
        end;
    end;
  end;

  procedure ReadObjectProp;
  var
    Value: TCnJSONValue;
    Sub: TObject;
  begin
    Value := Obj.ValueByName[string(PropInfo^.Name)];
    if Value <> nil then
    begin
      if Value is TCnJSONNull then
        SetObjectProp(Instance, string(PropInfo^.Name), nil)
      else if Value is TCnJSONObject then
      begin
        Sub := GetObjectProp(Instance, string(PropInfo^.Name));
        if Sub <> nil then
          Read(TPersistent(Sub), Value as TCnJSONObject);
      end;
    end;
  end;

begin
  if PropInfo^.SetProc <> nil then // ֻҪ��д
  begin
{$IFDEF FPC}
    PropType := PropInfo^.PropType;
{$ELSE}
    PropType := PropInfo^.PropType^;
{$ENDIF}
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        ReadOrdProp;
      tkString, tkLString, tkWString {$IFDEF UNICODE}, tkUString {$ENDIF} {$IFDEF FPC}, tkAString {$ENDIF}:
        ReadStrProp;
{$IFDEF FPC}
      tkBool:
        ReadBoolProp;
{$ENDIF}
      tkFloat:
        ReadFloatProp; // ʱ��������ʱ�����⴦���ڲ����ø�������
      tkInt64:
        ReadInt64Prop;
      tkClass:
        ReadObjectProp;
    end;
  end;
end;

{ TCnJSONWriter }

class procedure TCnJSONWriter.JSONObjectToFile(Obj: TCnJSONObject;
  const FileName: string; UseFormat: Boolean; Indent: Integer; Utf8Bom: Boolean);
var
  JSON: AnsiString;
begin
  JSON := CnJSONConstruct(Obj, UseFormat, Indent);
  JSONToFile(JSON, FileName, Utf8Bom);
end;

class procedure TCnJSONWriter.JSONToFile(const JSON: AnsiString;
  const FileName: string; Utf8Bom: Boolean);
var
  F: TFileStream;
begin
  // UTF8 ��ʽ�� AnsiString��д BOM ͷ�����ݵ��ļ�
  F := TFileStream.Create(FileName, fmCreate);
  try
    if Utf8Bom then
      F.Write(SCN_BOM_UTF8[0], SizeOf(SCN_BOM_UTF8));

    if Length(JSON) > 0 then
      F.Write(JSON[1], Length(JSON));
  finally
    F.Free;
  end;
end;

class procedure TCnJSONWriter.SaveToFile(Instance: TPersistent;
  const FileName: string; Utf8Bom: Boolean);
var
  JSON: AnsiString;
begin
  JSON := SaveToJSON(Instance);
  JSONToFile(JSON, FileName, Utf8Bom);
end;

class function TCnJSONWriter.SaveToJSON(Instance: TPersistent;
  UseFormat: Boolean): AnsiString;
var
  Obj: TCnJSONObject;
begin
  Obj := nil;

  try
    Obj := TCnJSONObject.Create;
    TCnJSONWriter.Write(Instance, Obj);
    Result := Obj.ToJSON(UseFormat);
  finally
    Obj.Free;
  end;
end;

class procedure TCnJSONWriter.Write(Instance: TPersistent; Obj: TCnJSONObject);
var
  PropCount: Integer;
  PropList: PPropList;
  I: Integer;
  PropInfo: PPropInfo;
  Arr: TCnJSONArray;
  Sub: TCnJSONObject;
begin
  PropCount := GetTypeData(Instance.ClassInfo)^.PropCount;
  if PropCount = 0 then
    Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo = nil then
        Break;

      if IsStoredProp(Instance, PropInfo) then
        WriteProperty(Instance, PropInfo, Obj)
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;

  if Instance is TCollection then
  begin
    Arr := TCnJSONArray.Create;
    Obj.AddPair('Items', Arr);

    for I := 0 to (Instance as TCollection).Count - 1 do
    begin
      Sub := TCnJSONObject.Create;
      Arr.AddChild(Sub);
      Write((Instance as TCollection).Items[I], Sub);
    end;
  end;
end;

class procedure TCnJSONWriter.WriteProperty(Instance: TPersistent;
  PropInfo: PPropInfo; Obj: TCnJSONObject);
var
  PropType: PTypeInfo;

{$IFDEF FPC}

  procedure WriteBoolProp;
  var
    Value: Boolean;
  begin
    Value := GetOrdProp(Instance, PropInfo) <> 0;
    WriteBooleanValue(Obj, string(PropInfo^.Name), Value);
  end;

{$ENDIF}

  procedure WriteStrProp;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
    WriteStringValue(Obj, string(PropInfo^.Name), Value);
  end;

  procedure WriteOrdProp;
  var
    Value: Longint;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    if Value <> PPropInfo(PropInfo)^.Default then
    begin
      case PropType^.Kind of
        tkInteger:
          WriteIntegerValue(Obj, string(PropInfo^.Name), Value);
        tkChar:
          WriteStringValue(Obj, string(PropInfo^.Name), Chr(Value));
        tkSet:
          WriteStringValue(Obj, string(PropInfo^.Name), GetSetProp(Instance, PPropInfo(PropInfo), True));
        tkEnumeration:
          begin
            if PropType = TypeInfo(Boolean) then
              WriteBooleanValue(Obj, string(PropInfo^.Name), Value <> 0)
            else
              WriteStringValue(Obj, string(PropInfo^.Name), GetEnumName(PropType, Value));
          end;
      end;
    end;
  end;

  procedure WriteFloatProp;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    WriteFloatValue(Obj, string(PropInfo^.Name), Value);
  end;

  procedure WriteInt64Prop;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    WriteInt64Value(Obj, string(PropInfo^.Name), Value);
  end;

  procedure WriteObjectProp;
  var
    Value: TObject;
    SubObj: TCnJSONObject;
  begin
    Value := GetObjectProp(Instance, PropInfo);
    if Value <> nil then
    begin
      if Value is TComponent then
      begin
        WriteStringValue(Obj, string(PropInfo^.Name), (Value as TComponent).Name);
      end
      else if Value is TPersistent then
      begin
        SubObj := TCnJSONObject.Create;
        Obj.AddPair(string(PropInfo^.Name), SubObj);
        Write(TPersistent(Value), SubObj);
      end;
    end
    else
      WriteNullValue(Obj, string(PropInfo^.Name));
  end;

begin
  if PropInfo^.GetProc <> nil then // ֻҪ�ɶ�
  begin
{$IFDEF FPC}
    PropType := PropInfo^.PropType;
{$ELSE}
    PropType := PropInfo^.PropType^;
{$ENDIF}
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        WriteOrdProp;
      tkString, tkLString, tkWString {$IFDEF UNICODE}, tkUString {$ENDIF} {$IFDEF FPC}, tkAString {$ENDIF}:
        WriteStrProp;
{$IFDEF FPC}
      tkBool:
        WriteBoolProp;
{$ENDIF}
      tkFloat:
        WriteFloatProp; // ʱ��������ʱ�����⴦���ڲ����ø�������
      tkInt64:
        WriteInt64Prop;
      tkClass:
        WriteObjectProp;
    end;
  end;
end;

class procedure TCnJSONWriter.WriteStringValue(Obj: TCnJSONObject; const Name,
  Value: string);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteBooleanValue(Obj: TCnJSONObject;
  const Name: string; Value: Boolean);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteFloatValue(Obj: TCnJSONObject;
  const Name: string; Value: Extended);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteInt64Value(Obj: TCnJSONObject;
  const Name: string; Value: Int64);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteIntegerValue(Obj: TCnJSONObject;
  const Name: string; Value: Integer);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteNullValue(Obj: TCnJSONObject;
  const Name: string);
begin
  Obj.AddPair(Name);
end;

{$IFDEF SUPPORT_FORMAT_SETTINGS}

initialization
  JSONFormatSettings.DecimalSeparator := '.';

{$ENDIF}
end.
