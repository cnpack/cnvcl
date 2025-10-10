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

unit CnStrings;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�CnStrings ʵ�ֵ�Ԫ������ AnsiStringList �Լ�һ�������Ӵ������㷨
*           ����֧�� Win32/64 �� Posix
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ����ƽ̨��PWinXPPro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7/2005 + C++Build 5/6
* ��    ע��AnsiStringList ��ֲ�� Delphi 7 �� StringList
* �����£�2025.08.14
*               ����һ���ָ����ָ���ȫƥ������ʵ�ֺ���
*           2022.10.25
*               ���� StringBuilder ��ʵ�֣�֧�� Ansi �� Unicode ģʽ
*           2022.04.25
*               ���������ַ����滻������֧������ƥ��
*           2017.01.09
*               ������ֲ�� Forrest Smith ���ַ���ģ��ƥ���㷨��
*               �����������һ��ƥ���ַ����ڿ�������⡣
*           2015.06.01
*               ���ӿ��������Ӵ��㷨 FastPosition
*           2013.03.04
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, {$IFDEF MSWINDOWS} Windows, {$ENDIF} CnNative;

const
  SCN_BOM_UTF8: array[0..2] of Byte = ($EF, $BB, $BF);

  SCN_BOM_UTF16_LE: array[0..1] of Byte = ($FF, $FE);

  SCN_BOM_UTF16_BE: array[0..1] of Byte = ($FE, $FF);

type
  TCnMatchMode = (mmStart, mmAnywhere, mmFuzzy);
  {* �ַ���ƥ��ģʽ����ͷƥ�䣬�м�ƥ�䣬ȫ��Χģ��ƥ��}

  TCnAnsiStrings = class;
  
  ICnStringsAdapter = interface
    ['{E32A5BD7-9A80-4DDE-83D7-2EE050BF476A}']
    procedure ReferenceStrings(S: TCnAnsiStrings);
    procedure ReleaseStrings;
  end;

  TCnAnsiStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator);

  TCnAnsiStrings = class(TPersistent)
  {* Ansi ��� TStrings�������� Unicode ����������ṩ Ansi ��� TStrings ����}
  private
    FDefined: TCnAnsiStringsDefined;
    FDelimiter: AnsiChar;
    FQuoteChar: AnsiChar;
    FNameValueSeparator: AnsiChar;
    FUpdateCount: Integer;
    FAdapter: ICnStringsAdapter;
    FUseSingleLF: Boolean;
    function GetCommaText: AnsiString;
    function GetDelimitedText: AnsiString;
    function GetName(Index: Integer): AnsiString;
    function GetValue(const Name: AnsiString): AnsiString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: AnsiString);
    procedure SetDelimitedText(const Value: AnsiString);
    procedure SetStringsAdapter(const Value: ICnStringsAdapter);
    procedure SetValue(const Name, Value: AnsiString);
    procedure WriteData(Writer: TWriter);
    function GetDelimiter: AnsiChar;
    procedure SetDelimiter(const Value: AnsiChar);
    function GetQuoteChar: AnsiChar;
    procedure SetQuoteChar(const Value: AnsiChar);
    function GetNameValueSeparator: AnsiChar;
    procedure SetNameValueSeparator(const Value: AnsiChar);
    function GetValueFromIndex(Index: Integer): AnsiString;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: AnsiString; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: AnsiString): AnsiString;
    function Get(Index: Integer): AnsiString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: AnsiString; virtual;
    procedure Put(Index: Integer; const S: AnsiString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: AnsiString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: AnsiString): Integer; virtual;
  public
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; virtual;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; virtual;
    procedure Append(const S: AnsiString);
    procedure AddStrings(Strings: TCnAnsiStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TCnAnsiStrings): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetText: PAnsiChar; virtual;
    function IndexOf(const S: AnsiString): Integer; virtual;
    function IndexOfName(const Name: AnsiString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: AnsiString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: AnsiString;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: AnsiString); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: AnsiString); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PAnsiChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: AnsiString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: AnsiChar read GetDelimiter write SetDelimiter;
    property DelimitedText: AnsiString read GetDelimitedText write SetDelimitedText;
    property Names[Index: Integer]: AnsiString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: AnsiChar read GetQuoteChar write SetQuoteChar;
    property Values[const Name: AnsiString]: AnsiString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: AnsiChar read GetNameValueSeparator write SetNameValueSeparator;
    property Strings[Index: Integer]: AnsiString read Get write Put; default;
    property Text: AnsiString read GetTextStr write SetTextStr;
    property StringsAdapter: ICnStringsAdapter read FAdapter write SetStringsAdapter;
    property UseSingleLF: Boolean read FUseSingleLF write FUseSingleLF;
    {* ���ӵ����ԣ����� GetTextStr ʱʹ�õĻ����Ƿ��ǵ��� #10 �����ǳ���� #13#10}
  end;

  TCnAnsiStringList = class;

  PCnAnsiStringItem = ^TCnAnsiStringItem;
  TCnAnsiStringItem = record
    FString: AnsiString;
    FObject: TObject;
  end;

  PCnAnsiStringItemList = ^TCnAnsiStringItemList;
  TCnAnsiStringItemList = array[0..MaxListSize div 2] of TCnAnsiStringItem;
  TCnAnsiStringListSortCompare = function(List: TCnAnsiStringList; Index1, Index2: Integer): Integer;

  TCnAnsiStringList = class(TCnAnsiStrings)
  {* Ansi ��� TStringList�������� Unicode ����������ṩ Ansi ��� TStringList ����}
  private
    FList: PCnAnsiStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TCnAnsiStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: AnsiString): Integer; override;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); virtual;
  public
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: AnsiString): Integer; override;
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TCnAnsiStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  PPCnAnsiHashItem = ^PCnAnsiHashItem;
  PCnAnsiHashItem = ^TCnAnsiHashItem;
  TCnAnsiHashItem = record
    Next: PCnAnsiHashItem;
    Key: AnsiString;
    Value: Integer;
  end;

  TCnAnsiStringHash = class
  private
    Buckets: array of PCnAnsiHashItem;
  protected
    function Find(const Key: AnsiString): PPCnAnsiHashItem;
    function HashOf(const Key: AnsiString): Cardinal; virtual;
  public
    constructor Create(Size: Cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: AnsiString; Value: Integer);
    procedure Clear;
    procedure Remove(const Key: AnsiString);
    function Modify(const Key: AnsiString; Value: Integer): Boolean;
    function ValueOf(const Key: AnsiString): Integer;
  end;

  TCnHashedAnsiStringList = class(TCnAnsiStringList)
  {* Ansi ��� THashedStringList�������� Unicode ����������ṩ Ansi ��� THashedStringList ����}
  private
    FValueHash: TCnAnsiStringHash;
    FNameHash: TCnAnsiStringHash;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    destructor Destroy; override;
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override;
  end;

  TCnStringBuilder = class
  {* �����ʽ���� StringBuilder����ʱֻ֧����ӣ���֧��ɾ����
     �� Unicode �汾֧�� string �� WideString��Unicode �汾֧�� AnsiString �� string}
  private
    FModeIsFromOut: Boolean;
    FOutMode: Boolean;
    FAnsiMode: Boolean;      // �� Unicode �汾Ĭ�� True��Unicode �汾Ĭ�� False���ɴ���ʱָ��
    FCharLength: Integer;    // ���ַ�Ϊ��λ�ĳ���
    FMaxCharCapacity: Integer;
{$IFDEF UNICODE}
    FAnsiData: AnsiString;   // AnsiMode True ʱʹ��
    FData: string;           // AnsiMode False ʱʹ��
{$ELSE}
    FData: string;           // AnsiMode True ʱʹ��
    FWideData: WideString;   // AnsiMode False ʱʹ��
{$ENDIF}
    function GetCharCapacity: Integer;
    procedure SetCharCapacity(const Value: Integer);
    procedure SetCharLength(const Value: Integer);
  protected
    procedure ExpandCharCapacity;
    {* ���� CharLength ��Ҫ������չ�ڲ��洢Ϊ CharLength * 2���� CharLength ̫����̶���չ Capacity �� 0.5 ��}

    function AppendString(const Value: string): TCnStringBuilder;
    {* �� string ��ӵ� FData�������Ƿ� Unicode �������ɵ����߸��� AnsiMode ���ơ�

       ������
         const Value: string              - ����ӵ��ַ���

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }
  public
    constructor Create; overload;
    {* ���캯�����ڲ�ʵ��Ĭ�� string}

    constructor Create(IsAnsi: Boolean); overload;
    {* ��ָ���ڲ��� Ansi ���� Wide �Ĺ��캯����

       ������
         IsAnsi: Boolean                  - ָ���ڲ��Ƿ�ʹ�� Ansi ģʽ

       ����ֵ�����ޣ�
    }

    destructor Destroy; override;
    {* ��������}

    procedure Clear;
    {* �������}

{$IFDEF UNICODE}
    function AppendAnsi(const Value: AnsiString): TCnStringBuilder;
    {* �� AnsiString ��ӵ� Unicode �����µ� FAnsiData���ɵ����߸��� AnsiMode ���ơ�

       ������
         const Value: AnsiString          - ����ӵĵ��ֽ��ַ���

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

{$ELSE}
    function AppendWide(const Value: WideString): TCnStringBuilder;
    {* �� WideString ��ӵ��� Unicode �����е� FWideData���ɵ����߸��� AnsiMode ���ơ�

       ������
         const Value: WideString          - ����ӵĿ��ַ���

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }
{$ENDIF}

    function Append(const Value: string): TCnStringBuilder; overload;
    {* ���ͨ���ַ����������������������� Append ������ڣ��ڲ����ݵ�ǰ�������Լ� AnsiMode �����ú���ʵ����ƴ�ӡ�

       ������
         const Value: string              - ����ӵ��ַ���

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: Boolean): TCnStringBuilder; overload;
    {* ���һ����ֵ��

       ������
         Value: Boolean                   - ����ӵĲ���ֵ

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function AppendChar(Value: Char): TCnStringBuilder;
    {* ���һ�ַ���ע�� Char �͵��ַ� String �ǵ�ͬ�ģ����������������ܺ� Append �� overload��

       ������
         Value: Char                      - ����ӵ��ַ�

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function AppendAnsiChar(Value: AnsiChar): TCnStringBuilder;
    {* ���һ���ֽ��ַ���

       ������
         Value: AnsiChar                  - ����ӵĵ��ֽ��ַ�

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function AppendWideChar(Value: WideChar): TCnStringBuilder;
    {* ���һ���ַ���

       ������
         Value: WideChar                  - ����ӵĿ��ַ�

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }


    function AppendCurrency(Value: Currency): TCnStringBuilder;
    {* ���һ Currency ֵ��ע�� Currency �ڵͰ汾 Delphi �к� Double �ǵ�ͬ�ģ�
       ���������������ܺ� Append �� overload��

       ������
         Value: Currency                  - ����ӵ� Currency ֵ

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }


    function Append(Value: Single): TCnStringBuilder; overload;
    {* ���һ�����ȸ�������

       ������
         Value: Single                    - ����ӵĵ����ȸ�����

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: Double): TCnStringBuilder; overload;
    {* ���һ˫���ȸ�������

       ������
         Value: Double                    - ����ӵ�˫���ȸ�����

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: ShortInt): TCnStringBuilder; overload;
    {* ���һ 8 λ�з���������

       ������
         Value: ShortInt                  - ����ӵ� 8 λ�з�������

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: SmallInt): TCnStringBuilder; overload;
    {* ���һ 16 λ�з���������

       ������
         Value: SmallInt                  - ����ӵ� 16 λ�з�������

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: Integer): TCnStringBuilder; overload;
    {* ���һ 32 λ�з���������

       ������
         Value: Integer                   - ����ӵ� 32 λ�з�������

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: Int64): TCnStringBuilder; overload;
    {* ���һ 64 λ�з���������

       ������
         Value: Int64                     - ����ӵ� 64 λ�з�������

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: Byte): TCnStringBuilder; overload;
    {* ���һ 8 λ�޷���������

       ������
         Value: Byte                      - ����ӵ� 8 λ�޷�������

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: Word): TCnStringBuilder; overload;
    {* ���һ 16 λ�޷���������

       ������
         Value: Word                      - ����ӵ� 16 λ�޷�������

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: Cardinal): TCnStringBuilder; overload;
    {* ���һ 32 λ�޷���������

       ������
         Value: Cardinal                  - ����ӵ� 32 λ�޷�������

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

{$IFDEF SUPPORT_UINT64}
    function Append(Value: UInt64): TCnStringBuilder; overload;
    {* ���һ 64 λ�޷���������

       ������
         Value: UInt64                    - ����ӵ� 64 λ�޷�������

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }
{$ENDIF}

    function Append(Value: TObject): TCnStringBuilder; overload;
    {* ���һ����

       ������
         Value: TObject                   - ����ӵĶ����ڲ�ʹ���� ToString������ʹ�ö���ʮ�����Ƶ�ַ

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: PAnsiChar): TCnStringBuilder; overload;
    {* ���һ���ֽ��ַ�����

       ������
         Value: PAnsiChar                 - ����ӵĵ��ֽ��ַ�����ַ

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(Value: Char; RepeatCount: Integer): TCnStringBuilder; overload;
    {* ���һ�ظ���������ͬ�ַ���

       ������
         Value: Char                      - ����ӵ��ַ�
         RepeatCount: Integer             - �ַ�����

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(const Value: string; StartIndex: Integer; Count: Integer): TCnStringBuilder; overload;
    {* ���һ�ַ������Ӵ���

       ������
         const Value: string              - ����ӵ��ַ���
         StartIndex: Integer              - ��ʼλ��
         Count: Integer                   - �ַ�����

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function Append(const AFormat: string; const Args: array of const): TCnStringBuilder; overload;
    {* ���һ��ʽ���ַ�����

       ������
         const AFormat: string            - ��ʽ�ַ���
         const Args: array of const       - ��ʽ�����б�

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function AppendLine: TCnStringBuilder; overload;
    {* ���һ���С�

       ������
         ���ޣ�

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function AppendLine(const Value: string): TCnStringBuilder; overload;
    {* ���һ�ַ��������ϻس�����

       ������
         const Value: string              - ����ӵ��ַ���

       ����ֵ��TCnStringBuilder           - ���ر����󹩽�һ����ӵ���
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* �������ݵ� string ��ʽ�������Ƿ� Unicode ������ֻҪ AnsiMode ��������� Unicode ֧��һ�¡�
       ���仰˵�� Unicode ������ AnsiMode Ϊ True ʱ�ŷ��� AnsiString��
       Unicode ������ AnsiMode Ϊ False ʱ�ŷ��� UnicodeString������������ؿա�

       ������
         ���ޣ�

       ����ֵ��string                     - �������ݵ��ַ�����ʽ
    }

    function ToAnsiString: AnsiString;
    {* ǿ�з������ݵ� AnsiString ��ʽ������ AnsiMode ��Ρ�
       ���� Unicode ������ʹ�ã����ڷ� Unicode ������ʹ�ã���ͬ�� ToString��

       ������
         ���ޣ�

       ����ֵ��AnsiString                 - �������ݵĵ��ֽ��ַ�����ʽ
    }

    function ToWideString: WideString;
    {* ǿ�з������ݵ� WideString ��ʽ������ AnsiMode ��Ρ�
       ���ڷ� Unicode ������ʹ�ã����� Unicode ������ʹ�ã���ͬ�� ToString��

       ������
         ���ޣ�

       ����ֵ��WideString                 - �������ݵĿ��ַ�����ʽ
    }

    property CharCapacity: Integer read GetCharCapacity write SetCharCapacity;
    {* ���ַ�Ϊ��λ���ڲ�������������}
    property CharLength: Integer read FCharLength write SetCharLength;
    {* ���ַ�Ϊ��λ���ڲ��Ѿ�ƴ�յ����ݳ���}
    property MaxCharCapacity: Integer read FMaxCharCapacity;
    {* ���ַ�Ϊ��λ�Ŀ����õ������������}
  end;

  TCnReplaceFlags = set of (crfReplaceAll, crfIgnoreCase, crfWholeWord);
  {* �ַ����滻���}

{$IFNDEF COMPILER7_UP}

function PosEx(const SubStr: string; const S: string; Offset: Cardinal = 1): Integer;
{* D5/6 BCB5/6 ���� StrUtils ��Ԫ�Ĵ˺�������ֲ�� PosEx ���������ʹ����ο� PosEx ������

   ������
     const SubStr: string                 - �����ҵ��Ӵ�
     const S: string                      - ԭ�ַ���
     Offset: Cardinal                     - ���ҵ���ʼƫ����

   ����ֵ��Integer                        - ���ش���ʼƫ�������һ�γ����Ӵ���λ��
}

{$ENDIF}

function FastPosition(const Str: PChar; const Pattern: PChar; FromIndex: Integer = 0): Integer;
{* ���������Ӵ������� Pattern �� Str �еĵ�һ�γ��ֵ������ţ����򷵻� -1��

   ������
     const Str: PChar                     - �������������ַ���
     const Pattern: PChar                 - ��ƥ����Ӵ�
     FromIndex: Integer                   - �Ӻδ���ʼ����

   ����ֵ��Integer                        - ����ƥ��ĵ�һ�γ��ֵ������ţ����򷵻� -1
}

function FuzzyMatchStr(const Pattern: string; const Str: string; MatchedIndexes: TList = nil;
  CaseSensitive: Boolean = False): Boolean;
{* ģ��ƥ���Ӵ���MatchedIndexes �з��� Str ��ƥ����±�š�

   ������
     const Pattern: string                - ��ƥ����Ӵ�
     const Str: string                    - �������������ַ���
     MatchedIndexes: TList                - �����ַ����и��ַ�ƥ����±��
     CaseSensitive: Boolean               - �����Ƿ����ִ�Сд

   ����ֵ��Boolean                        - �����Ƿ���ģ��ƥ�������
}

function FuzzyMatchStrWithScore(const Pattern: string; const Str: string; out Score: Integer;
  MatchedIndexes: TList = nil; CaseSensitive: Boolean = False): Boolean;
{* ģ��ƥ���Ӵ���Score ����ƥ��̶ȣ�MatchedIndexes �з��� Str ��ƥ����±�ţ�
   ע�� Score �ıȽ�ֻ���Ӵ��Լ���Сдһ��ʱ�������塣

   ������
     const Pattern: string                - ��ƥ����Ӵ�
     const Str: string                    - �������������ַ���
     out Score: Integer                   - ����ƥ��̶�����
     MatchedIndexes: TList                - �����ַ����и��ַ�ƥ����±��
     CaseSensitive: Boolean               - �����Ƿ����ִ�Сд

   ����ֵ��Boolean                        - �����Ƿ���ģ��ƥ�������
}

function AnyWhereSepMatchStr(const Pattern: string; const Str: string; SepContainer: TStringList;
  MatchedIndexes: TList = nil; CaseSensitive: Boolean = False; SepChar: Char = ' '): Boolean;
{* �ָ��Ӵ��������ƥ���Ӵ���Ҳ���ǰ� Pattern �� SepChar ���ֳɶ���ַ��������ƥ�䣬ȫ��ƥ��ŷ���ƥ�䡣
   MatchedIndexes �з��� Str ��ƥ����±�ţ�SepContainer ����紫��� TStringList �Լ��ٴ���������

   ������
     const Pattern: string                - ��ƥ����Ӵ�
     const Str: string                    - �������������ַ���
     SepContainer: TStringList;           - ��紫��� TStringList �Լ����ڲ���������
     MatchedIndexes: TList                - �����ַ����и��ַ�ƥ����±��
     CaseSensitive: Boolean               - �����Ƿ����ִ�Сд

   ����ֵ��Boolean                        - �����Ƿ�ƥ��ɹ�
}

function CnStringReplace(const S: string; const OldPattern: string;
  const NewPattern: string; Flags: TCnReplaceFlags): string;
{* ֧������ƥ����ַ����滻���� Unicode ��� Unicode �������¶���Ч��

   ������
     const S: string                      - ���滻���ַ���
     const OldPattern: string             - ���滻���ַ�������
     const NewPattern: string             - �滻���ַ���������
     Flags: TCnReplaceFlags               - �滻��ǣ�֧������ƥ��

   ����ֵ��string                         - �����ַ����滻���
}

{$IFDEF UNICODE}

function CnStringReplaceA(const S: AnsiString; const OldPattern: AnsiString;
  const NewPattern: AnsiString; Flags: TCnReplaceFlags): AnsiString;
{* ֧������ƥ��� Ansi �ַ����滻���� Unicode ����������Ч��

   ������
     const S: AnsiString                  - ���滻�ĵ��ֽ��ַ���
     const OldPattern: AnsiString         - ���滻�ĵ��ֽ��ַ�������
     const NewPattern: AnsiString         - �滻�ĵ��ֽ��ַ���������
     Flags: TCnReplaceFlags               - �滻��ǣ�֧������ƥ��

   ����ֵ��AnsiString                     - ���ص��ֽ��ַ����滻���
}

{$ELSE}

function CnStringReplaceW(const S: WideString; const OldPattern: WideString;
  const NewPattern: WideString; Flags: TCnReplaceFlags): WideString;
{* ֧������ƥ��� Wide �ַ����滻���ڷ� Unicode ����������Ч��

   ������
     const S: WideString                  - ���滻�Ŀ��ַ���
     const OldPattern: WideString         - ���滻�Ŀ��ַ�������
     const NewPattern: WideString         - �滻�Ŀ��ַ���������
     Flags: TCnReplaceFlags               - �滻��ǣ�֧������ƥ��

   ����ֵ��WideString                     - ���ؿ��ַ����滻���
}

{$ENDIF}

function CnPosEx(const SubStr, S: string; CaseSensitive: Boolean; WholeWords:
  Boolean; StartCount: Integer = 1): Integer;
{* ��ǿ���ַ������Һ�����֧�ֲ��ҵڼ������׸��� StartCount Ϊ 1}

function NativeStringToUIString(const Str: string): string;
{* Lazarus/FPC �� Ansi ģʽר�ã���Ϊ Lazarus/FPC �� Ansi ģʽ�ºͽ����йص��ַ����� Utf8 ��ʽ��
   �������ڲ�����ͨ�ַ�������� Ansi �� Utf16��������һ�η�װת����}

function UIStringToNativeString(const Str: string): string;
{* Lazarus/FPC �� Ansi ģʽר�ã���Ϊ Lazarus/FPC �� Ansi ģʽ�ºͽ����йص��ַ����� Utf8 ��ʽ��
   �������ڲ�����ͨ�ַ�������� Ansi �� Utf16��������һ�η�װת����}

implementation

uses
  CnWideStrings;

const
  SLineBreak = #13#10;
  SLineBreakLF = #10;
  STRING_BUILDER_DEFAULT_CAPACITY = 16;

resourcestring
  SDuplicateString = 'AnsiString list does not allow duplicates';
  SListIndexError = 'AnsiString List index out of bounds (%d)';
  SSortedListError = 'Operation not allowed on sorted AnsiString list';
  SListCapacityError = 'Error New Capacity or Length Value %d';

function NativeStringToUIString(const Str: string): string;
begin
{$IFDEF FPC}
  Result := CnAnsiToUtf82(Str);
{$ELSE}
  Result := Str;
{$ENDIF}
end;

function UIStringToNativeString(const Str: string): string;
begin
{$IFDEF FPC}
  Result := CnUtf8ToAnsi2(Str);
{$ELSE}
  Result := Str;
{$ENDIF}
end;

{$IFNDEF COMPILER7_UP}

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

{$ENDIF}

// ���������Ӵ������� Pattern �� Str �еĵ�һ�γ��ֵ������ţ����򷵻� -1
function FastPosition(const Str, Pattern: PChar; FromIndex: Integer): Integer;
var
  C: Char;
  I, L, X, Y, PLen, SLen: Integer;
  BCS: array[0..255] of Integer;
begin
  Result := -1;
  if (Str = nil) or (Pattern = nil) then
    Exit;

  PLen := StrLen(Pattern);
  if PLen = 0 then
    Exit;
  SLen := StrLen(Str);

  // ����ǵ��ַ�����ģʽ
  if PLen = 1 then
  begin
    for I := FromIndex to SLen - 1 do
    begin
      if Str[I] = Pattern[0] then
      begin
        Result := I;
        Exit;
      end;
    end;
    Exit;
  end;

  // ������Ծ����
  for I := Low(BCS) to High(BCS) do
    BCS[I] := PLen;

  for I := 0 to PLen - 2 do
  begin
    C := Pattern[I];
    L := Ord(C) and $FF;
    if PLen - I - 1 < BCS[L] then
      BCS[L] := PLen - I - 1;
  end;

  // �ٽ�������
  I := FromIndex + PLen - 1;
  while I < SLen do
  begin
    X := I;
    Y := PLen - 1;
    while True do
    begin
      if Pattern[Y] <> Str[X] then
      begin
        Inc(I, BCS[Ord(Str[X]) and $FF]);
        Break;
      end;

      if Y = 0 then
      begin
        Result := X;
        Exit;
      end;

      Dec(X);
      Dec(Y);
    end;
  end;
end;

{$WARNINGS OFF}

function LowChar(AChar: Char): Char; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if AChar in ['A'..'Z'] then
    Result := Chr(Ord(AChar) + 32)
  else
    Result := AChar;
end;

// ģ��ƥ���Ӵ�
function FuzzyMatchStr(const Pattern: string; const Str: string;
  MatchedIndexes: TList; CaseSensitive: Boolean): Boolean;
var
  PIdx, SIdx: Integer;
begin
  Result := False;
  if (Pattern = '') or (Str = '') then
    Exit;

  PIdx := 1;
  SIdx := 1;
  if MatchedIndexes <> nil then
    MatchedIndexes.Clear;

  if CaseSensitive then
  begin
    while (PIdx <= Length(Pattern)) and (SIdx <= Length(Str)) do
    begin
      if Pattern[PIdx] = Str[SIdx] then
      begin
        Inc(PIdx);
        if MatchedIndexes <> nil then
          MatchedIndexes.Add(Pointer(SIdx));
      end;
      Inc(SIdx);
    end;
  end
  else
  begin
    while (PIdx <= Length(Pattern)) and (SIdx <= Length(Str)) do
    begin
      if LowChar(Pattern[PIdx]) = LowChar(Str[SIdx]) then
      begin
        Inc(PIdx);
        if MatchedIndexes <> nil then
          MatchedIndexes.Add(Pointer(SIdx));
      end;
      Inc(SIdx);
    end;
  end;
  Result := PIdx > Length(Pattern);
end;

// ģ��ƥ���Ӵ���Score ����ƥ��̶ȣ�ע�� Score �ıȽ�ֻ���Ӵ��Լ���Сдһ��ʱ��������
function FuzzyMatchStrWithScore(const Pattern: string; const Str: string;
  out Score: Integer; MatchedIndexes: TList; CaseSensitive: Boolean): Boolean;
const
  ADJACENCY_BONUS = 4;               // ÿ��һ���ַ��Ľ���ƥ��ʱ�ӷ�
  SEPARATOR_BONUS = 10;              // ÿ��һ���ַ�ƥ�䷢����һ���ָ����ź�ļӷ�
  CAMEL_BONUS = 5;                   // ǰһ��ƥ����Сд�������Ǵ�дʱ�ӷ�
  LEADING_LETTER_PENALTY = -3;       // ��һ��ƥ�����ĸԽ��ĸ����Խ�۷�
  MAX_LEADING_LETTER_PENALTY = -9;   // ��һ��ƥ�����ĸ������󣬷ⶥֻ����ô���
  UNMATCHED_LETTER_PENALTY = -1;     // ��ƥ��Ŀ۷�
  START_BONUS = 6;
var
  PIdx, SIdx: Integer;
  PrevMatch, PrevLow, PrevSep: Boolean;
  BestLetterPtr: PChar;
  BestLetterScore, NewScore, Penalty: Integer;
  PatternLetter, StrLetter: Char; // �ֱ����������Ӵ���ĸ�����ַ�
  ThisMatch, Rematch, Advanced, PatternRepeat: Boolean;
begin
  Score := 0;
  Result := False;
  if (Pattern = '') or (Str = '') then
    Exit;

  if MatchedIndexes <> nil then
    MatchedIndexes.Clear;

  PrevMatch := False;
  PrevLow := False;
  PrevSep := True;

  PIdx := 1;
  SIdx := 1;

  BestLetterPtr := nil;
  BestLetterScore := 0;

  while SIdx <= Length(Str) do // SIdx ��ĸ������λ�ã�1 ��ʼ
  begin
    if PIdx <= Length(Pattern) then
      PatternLetter := Pattern[PIdx]
    else
      PatternLetter := #0;
    StrLetter := Str[SIdx];

    if CaseSensitive then
    begin
      ThisMatch := (PatternLetter <> #0) and (PatternLetter = StrLetter);
      Rematch := (BestLetterPtr <> nil) and (BestLetterPtr^ = StrLetter);
      Advanced := ThisMatch and (BestLetterPtr <> nil);
      PatternRepeat := (BestLetterPtr <> nil) and (PatternLetter <> #0) and (BestLetterPtr^ = PatternLetter);
    end
    else
    begin
      ThisMatch := (PatternLetter <> #0) and (LowChar(PatternLetter) = LowChar(StrLetter));
      Rematch := (BestLetterPtr <> nil) and (LowChar(BestLetterPtr^) = LowChar(StrLetter));
      Advanced := ThisMatch and (BestLetterPtr <> nil);
      PatternRepeat := (BestLetterPtr <> nil) and (PatternLetter <> #0) and (LowChar(BestLetterPtr^) = LowChar(PatternLetter));
    end;

    if ThisMatch and (MatchedIndexes <> nil) then
    begin
      MatchedIndexes.Add(Pointer(SIdx));
      if SIdx <= START_BONUS then        // ��߿�ĸ��ǰͷ����ƥ���ַ��ķ���
        Inc(Score, (START_BONUS - SIdx + 1) * 2);
    end;

    if Advanced or PatternRepeat then
    begin
      Inc(Score, BestLetterScore);
      BestLetterPtr := nil;
      BestLetterScore := 0;
    end;

    if ThisMatch or Rematch then
    begin
      NewScore := 0;
      if PIdx = 1 then
      begin
        Penalty := LEADING_LETTER_PENALTY * (SIdx - 1); // ��ͷ��ƥ�䲻�۷�
        if Penalty < MAX_LEADING_LETTER_PENALTY then
          Penalty := MAX_LEADING_LETTER_PENALTY;

        Inc(Score, Penalty);
      end;

      if PrevMatch then
        Inc(NewScore, ADJACENCY_BONUS);
      if PrevSep then
        Inc(NewScore, SEPARATOR_BONUS);
      if PrevLow and (strLetter in ['A'..'Z']) then
        Inc(NewScore, CAMEL_BONUS);

      if ThisMatch then
        Inc(PIdx);

      if NewScore >= BestLetterScore then
      begin
        if BestLetterPtr <> nil then
          Inc(Score, UNMATCHED_LETTER_PENALTY);
        BestLetterPtr := @(Str[SIdx]);
        BestLetterScore := NewScore;
      end;
      PrevMatch := True;
    end
    else
    begin
      Inc(Score, UNMATCHED_LETTER_PENALTY);
      PrevMatch := False;
    end;

    PrevLow := StrLetter in ['a'..'z'];
    PrevSep := strLetter in ['_', ' ', '/', '\', '.'];

    Inc(SIdx);
  end;

  if BestLetterPtr <> nil then
    Inc(Score, BestLetterScore);

  Result := PIdx > Length(Pattern);
end;

function MatchedIndexesCompare(Item1, Item2: Pointer): Integer;
var
  R1, R2: Integer;
begin
  R1 := Integer(Item1);
  R2 := Integer(Item2);
  Result := R1 - R2;
end;

function AnyWhereSepMatchStr(const Pattern: string; const Str: string; SepContainer: TStringList;
  MatchedIndexes: TList; CaseSensitive: Boolean; SepChar: Char): Boolean;
var
  IsNil: Boolean;
  D, I, J: Integer;
  ToFind: string;
  SepChars: TSysCharSet;
begin
  Result := False;

  if Pos(SepChar, Pattern) <= 0 then
  begin
    // û�и����ַ����ɱ�� Pos
    if CaseSensitive then
      D := Pos(Pattern, Str)
    else
      D := Pos(UpperCase(Pattern), UpperCase(Str));

    if D > 0 then
    begin
      Result := True;
      if MatchedIndexes <> nil then
      begin
        MatchedIndexes.Clear;
        for I := 0 to Length(Pattern) - 1 do
          MatchedIndexes.Add(Pointer(D + I));
      end;
    end;
  end
  else
  begin
    IsNil := SepContainer = nil;
    if IsNil then
      SepContainer := TStringList.Create
    else
      SepContainer.Clear;

    try
      SepChars := [];
      Include(SepChars, AnsiChar(SepChar));
      if CaseSensitive then
      begin
        ExtractStrings(SepChars, [], PChar(Pattern), SepContainer);
        ToFind := Str;
      end
      else
      begin
        ExtractStrings(SepChars, [], PChar(UpperCase(Pattern)), SepContainer);
        ToFind := UpperCase(Str);
      end;

      if MatchedIndexes <> nil then
        MatchedIndexes.Clear;
      for I := 0 to SepContainer.Count - 1 do
      begin
        D := Pos(SepContainer[I], ToFind);
        if D <= 0 then
        begin
          if MatchedIndexes <> nil then
            MatchedIndexes.Clear;
          Exit;
        end
        else
        begin
          if MatchedIndexes <> nil then
          begin
            for J := 0 to Length(SepContainer[I]) - 1 do
              MatchedIndexes.Add(Pointer(D + J));
          end;
        end;
      end;

      if (MatchedIndexes <> nil) and (MatchedIndexes.Count > 1) then
        MatchedIndexes.Sort(MatchedIndexesCompare);
      Result := True;
    finally
      if IsNil then
        SepContainer.Free;
    end;
  end;
end;

{ TCnAnsiStrings }

destructor TCnAnsiStrings.Destroy;
begin
  StringsAdapter := nil;
  inherited Destroy;
end;

function TCnAnsiStrings.Add(const S: AnsiString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TCnAnsiStrings.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TCnAnsiStrings.Append(const S: AnsiString);
begin
  Add(S);
end;

procedure TCnAnsiStrings.AddStrings(Strings: TCnAnsiStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TCnAnsiStrings.Assign(Source: TPersistent);
begin
  if Source is TCnAnsiStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TCnAnsiStrings(Source).FDefined;
      FNameValueSeparator := TCnAnsiStrings(Source).FNameValueSeparator;
      FQuoteChar := TCnAnsiStrings(Source).FQuoteChar;
      FDelimiter := TCnAnsiStrings(Source).FDelimiter;
      AddStrings(TCnAnsiStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TCnAnsiStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TCnAnsiStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TCnAnsiStrings then
        Result := not Equals(TCnAnsiStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TCnAnsiStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TCnAnsiStrings.Equals(Strings: TCnAnsiStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TCnAnsiStrings.Error(const Msg: AnsiString; Data: Integer);

{$IFDEF MSWINDOWS}
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  raise EStringListError.CreateFmt(string(Msg), [Data]) at ReturnAddr;
{$ELSE}
  raise EStringListError.CreateFmt(string(Msg), [Data]);
{$ENDIF}
end;

procedure TCnAnsiStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(AnsiString(LoadResString(Msg)), Data);
end;

procedure TCnAnsiStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TCnAnsiStrings.ExtractName(const S: AnsiString): AnsiString;
var
  P: Integer;
begin
  Result := S;
  P := AnsiPos(string(NameValueSeparator), string(S));
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TCnAnsiStrings.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

function TCnAnsiStrings.GetCommaText: AnsiString;
var
  LOldDefined: TCnAnsiStringsDefined;
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

function TCnAnsiStrings.GetDelimitedText: AnsiString;
var
  S: AnsiString;
  P: PAnsiChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PAnsiChar(S);
      while not (P^ in [#0..' ', QuoteChar, Delimiter]) do
      {$IFDEF MSWINDOWS}
        P := CharNextA(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
      if (P^ <> #0) then S := AnsiString(AnsiQuotedStr(string(S), Char(QuoteChar)));
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TCnAnsiStrings.GetName(Index: Integer): AnsiString;
begin
  Result := ExtractName(Get(Index));
end;

function TCnAnsiStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TCnAnsiStrings.GetText: PAnsiChar;
begin
  Result := StrNew(PAnsiChar(GetTextStr));
end;

function TCnAnsiStrings.GetTextStr: AnsiString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
begin
  Count := GetCount;
  Size := 0;

  if FUseSingleLF then
    LB := SLineBreakLF
  else
    LB := SLineBreak;

  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

function TCnAnsiStrings.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TCnAnsiStrings.IndexOf(const S: AnsiString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TCnAnsiStrings.IndexOfName(const Name: AnsiString): Integer;
var
  P: Integer;
  S: AnsiString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := AnsiPos(string(NameValueSeparator), string(S));
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TCnAnsiStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TCnAnsiStrings.InsertObject(Index: Integer; const S: AnsiString;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TCnAnsiStrings.LoadFromFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnAnsiStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

procedure TCnAnsiStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCnAnsiStrings.Put(Index: Integer; const S: AnsiString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TCnAnsiStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TCnAnsiStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do Add(AnsiString(Reader.ReadString));
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TCnAnsiStrings.SaveToFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(string(FileName), fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnAnsiStrings.SaveToStream(Stream: TStream);
var
  S: AnsiString;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TCnAnsiStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

procedure TCnAnsiStrings.SetCommaText(const Value: AnsiString);
begin
  Delimiter := ',';
  QuoteChar := '"';
  SetDelimitedText(Value);
end;

procedure TCnAnsiStrings.SetStringsAdapter(const Value: ICnStringsAdapter);
begin
  if FAdapter <> nil then FAdapter.ReleaseStrings;
  FAdapter := Value;
  if FAdapter <> nil then FAdapter.ReferenceStrings(Self);
end;

procedure TCnAnsiStrings.SetText(Text: PAnsiChar);
begin
  SetTextStr(Text);
end;

procedure TCnAnsiStrings.SetTextStr(const Value: AnsiString);
var
  P, Start: PAnsiChar;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13]) do Inc(P);
        SetString(S, Start, P - Start);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TCnAnsiStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TCnAnsiStrings.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TCnAnsiStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(string(Get(I)));
  Writer.WriteListEnd;
end;

procedure TCnAnsiStrings.SetDelimitedText(const Value: AnsiString);
var
  P, P1: PAnsiChar;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Clear;
    P := PAnsiChar(Value);
    while P^ in [#1..' '] do
    {$IFDEF MSWINDOWS}
      P := CharNextA(P);
    {$ELSE}
      Inc(P);
    {$ENDIF}
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while (P^ > ' ') and (P^ <> Delimiter) do
        {$IFDEF MSWINDOWS}
          P := CharNextA(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}
        SetString(S, P1, P - P1);
      end;
      Add(S);
      while P^ in [#1..' '] do
      {$IFDEF MSWINDOWS}
        P := CharNextA(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
      if P^ = Delimiter then
      begin
        P1 := P;
        {$IFDEF MSWINDOWS}
        if CharNextA(P1)^ = #0 then
        {$ELSE}
        Inc(P1);
        if P1^ = #0 then
        {$ENDIF}
          Add('');
        repeat
          {$IFDEF MSWINDOWS}
          P := CharNextA(P);
          {$ELSE}
          Inc(P);
          {$ENDIF}
        until not (P^ in [#1..' ']);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TCnAnsiStrings.GetDelimiter: AnsiChar;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TCnAnsiStrings.GetQuoteChar: AnsiChar;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

procedure TCnAnsiStrings.SetDelimiter(const Value: AnsiChar);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TCnAnsiStrings.SetQuoteChar(const Value: AnsiChar);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

function TCnAnsiStrings.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  Result := AnsiCompareText(string(S1), string(S2));
end;

function TCnAnsiStrings.GetNameValueSeparator: AnsiChar;
begin
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

procedure TCnAnsiStrings.SetNameValueSeparator(const Value: AnsiChar);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

function TCnAnsiStrings.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TCnAnsiStrings.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{ TCnAnsiStringList }

destructor TCnAnsiStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TCnAnsiStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

function TCnAnsiStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TCnAnsiStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCnAnsiStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TCnAnsiStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TCnAnsiStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TCnAnsiStringItem));
  Changed;
end;

procedure TCnAnsiStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TCnAnsiStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TCnNativeInt;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := TCnNativeInt(Item1^.FString);
  TCnNativeInt(Item1^.FString) := TCnNativeInt(Item2^.FString);
  TCnNativeInt(Item2^.FString) := Temp;
  Temp := TCnNativeInt(Item1^.FObject);
  TCnNativeInt(Item1^.FObject) := TCnNativeInt(Item2^.FObject);
  TCnNativeInt(Item2^.FObject) := Temp;
end;

function TCnAnsiStringList.Find(const S: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TCnAnsiStringList.Get(Index: Integer): AnsiString;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TCnAnsiStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TCnAnsiStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TCnAnsiStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TCnAnsiStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnAnsiStringList.IndexOf(const S: AnsiString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TCnAnsiStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

procedure TCnAnsiStringList.InsertObject(Index: Integer; const S: AnsiString;
  AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TCnAnsiStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TCnAnsiStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TCnAnsiStringList.Put(Index: Integer; const S: AnsiString);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TCnAnsiStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TCnAnsiStringList.QuickSort(L, R: Integer; SCompare: TCnAnsiStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TCnAnsiStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TCnAnsiStringItem));
  FCapacity := NewCapacity;
end;

procedure TCnAnsiStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TCnAnsiStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListCompareStrings(List: TCnAnsiStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList^[Index1].FString,
                                List.FList^[Index2].FString);
end;

procedure TCnAnsiStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TCnAnsiStringList.CustomSort(Compare: TCnAnsiStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TCnAnsiStringList.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(string(S1), string(S2))
  else
    Result := AnsiCompareText(string(S1), string(S2));
end;

procedure TCnAnsiStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

{ TCnAnsiStringHash }

procedure TCnAnsiStringHash.Add(const Key: AnsiString; Value: Integer);
var
  Hash: Integer;
  Bucket: PCnAnsiHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TCnAnsiStringHash.Clear;
var
  I: Integer;
  P, N: PCnAnsiHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

constructor TCnAnsiStringHash.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
end;

destructor TCnAnsiStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCnAnsiStringHash.Find(const Key: AnsiString): PPCnAnsiHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TCnAnsiStringHash.HashOf(const Key: AnsiString): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(Key[I]);
end;

function TCnAnsiStringHash.Modify(const Key: AnsiString; Value: Integer): Boolean;
var
  P: PCnAnsiHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

procedure TCnAnsiStringHash.Remove(const Key: AnsiString);
var
  P: PCnAnsiHashItem;
  Prev: PPCnAnsiHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;

function TCnAnsiStringHash.ValueOf(const Key: AnsiString): Integer;
var
  P: PCnAnsiHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;

{ TCnHashedAnsiStringList }

procedure TCnHashedAnsiStringList.Changed;
begin
  inherited Changed;
  FValueHashValid := False;
  FNameHashValid := False;
end;

destructor TCnHashedAnsiStringList.Destroy;
begin
  FValueHash.Free;
  FNameHash.Free;
  inherited Destroy;
end;

function TCnHashedAnsiStringList.IndexOf(const S: AnsiString): Integer;
begin
  UpdateValueHash;
  if not CaseSensitive then
    Result :=  FValueHash.ValueOf(AnsiString(AnsiUpperCase(string(S))))
  else
    Result :=  FValueHash.ValueOf(S);
end;

function TCnHashedAnsiStringList.IndexOfName(const Name: AnsiString): Integer;
begin
  UpdateNameHash;
  if not CaseSensitive then
    Result := FNameHash.ValueOf(AnsiString(AnsiUpperCase(string(Name))))
  else
    Result := FNameHash.ValueOf(Name);
end;

procedure TCnHashedAnsiStringList.UpdateNameHash;
var
  I: Integer;
  P: Integer;
  Key: AnsiString;
begin
  if FNameHashValid then Exit;
  
  if FNameHash = nil then
    FNameHash := TCnAnsiStringHash.Create
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
  begin
    Key := Get(I);
    P := AnsiPos('=', string(Key));
    if P <> 0 then
    begin
      if not CaseSensitive then
        Key := AnsiString(AnsiUpperCase(string(Copy(Key, 1, P - 1))))
      else
        Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, I);
    end;
  end;
  FNameHashValid := True;
end;

procedure TCnHashedAnsiStringList.UpdateValueHash;
var
  I: Integer;
begin
  if FValueHashValid then Exit;
  
  if FValueHash = nil then
    FValueHash := TCnAnsiStringHash.Create
  else
    FValueHash.Clear;
  for I := 0 to Count - 1 do
    if not CaseSensitive then
      FValueHash.Add(AnsiString(AnsiUpperCase(string(Self[I]))), I)
    else
      FValueHash.Add(Self[I], I);
  FValueHashValid := True;
end;

// �ж�һ���ַ��Ƿ�����ƥ��ķָ���
function IsSepChar(AChar: Char): Boolean;
begin
{$IFDEF UNICODE}
  Result := not CharInSet(AChar, ['0'..'9', 'A'..'Z', 'a'..'z', '_']);
{$ELSE}
  Result := not (AChar in ['0'..'9', 'A'..'Z', 'a'..'z', '_']);
{$ENDIF}
end;

function IsSepCharA(AChar: AnsiChar): Boolean;
begin
  Result := not (AChar in ['0'..'9', 'A'..'Z', 'a'..'z', '_']);
end;

function IsSepCharW(AChar: WideChar): Boolean;
begin
  Result := (Ord(AChar) < 127) and not (AnsiChar(AChar) in ['0'..'9', 'A'..'Z', 'a'..'z', '_']);
end;

function CnStringReplace(const S, OldPattern, NewPattern: string;
  Flags: TCnReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset, TailOffset: Integer;
  IsWhole: Boolean;
begin
  if crfIgnoreCase in Flags then
  begin
{$IFDEF UNICODE}
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
{$ELSE}
    SearchStr := AnsiUpperCase(S);
    Patt := AnsiUpperCase(OldPattern);
{$ENDIF}
  end
  else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;

  NewStr := S;
  Result := '';

  while SearchStr <> '' do
  begin
{$IFDEF UNICODE}
    Offset := Pos(Patt, SearchStr);
{$ELSE}
    Offset := AnsiPos(Patt, SearchStr);
{$ENDIF}
    IsWhole := True;
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end
    else if crfWholeWord in Flags then
    begin
      // �ҵ����Ӵ�����Ҫ����ƥ�䣬����������жϣ���������
      // ��ͷ��ͷ�Ƿָ���������β��β�Ƿָ������������
      if (Offset > 1) and not IsSepChar(SearchStr[Offset - 1]) then
        IsWhole := False
      else
      begin
        TailOffset := Offset + Length(Patt); // ָ��ƥ����һ���ַ�
        if (TailOffset <= Length(SearchStr)) and not IsSepChar(SearchStr[TailOffset]) then
          IsWhole := False;
      end;

      // �õ����Ƿ�����ƥ��Ľ���
    end;

    if not (crfWholeWord in Flags) or IsWhole then // ��ͨƥ�������ƥ����
    begin
      // �滻һ��
      Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
      if not (crfReplaceAll in Flags) then
      begin
        Result := Result + NewStr;
        Break;
      end;
    end
    else // ����ƥ���Ҫ���£�δ����ƥ�䣬�����滻
    begin
      Result := Result + Copy(NewStr, 1, Offset - 1) + OldPattern; // ע������� OldePattern�������滻
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

{$IFDEF UNICODE}

function CnStringReplaceA(const S, OldPattern, NewPattern: AnsiString;
  Flags: TCnReplaceFlags): AnsiString;
var
  SearchStr, Patt, NewStr: AnsiString;
  Offset, TailOffset: Integer;
  IsWhole: Boolean;
begin
  if crfIgnoreCase in Flags then
  begin
    SearchStr := AnsiUpperCase(S);
    Patt := AnsiUpperCase(OldPattern);
  end
  else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;

  NewStr := S;
  Result := '';

  while SearchStr <> '' do
  begin
    Offset := AnsiPos(Patt, SearchStr);
    IsWhole := True;
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end
    else if crfWholeWord in Flags then
    begin
      // �ҵ����Ӵ�����Ҫ����ƥ�䣬����������жϣ���������
      // ��ͷ��ͷ�Ƿָ���������β��β�Ƿָ������������
      if (Offset > 1) and not IsSepCharA(SearchStr[Offset - 1]) then
        IsWhole := False
      else
      begin
        TailOffset := Offset + Length(Patt); // ָ��ƥ����һ���ַ�
        if (TailOffset <= Length(SearchStr)) and not IsSepCharA(SearchStr[TailOffset]) then
          IsWhole := False;
      end;

      // �õ����Ƿ�����ƥ��Ľ���
    end;

    if not (crfWholeWord in Flags) or IsWhole then // ��ͨƥ�������ƥ����
    begin
      // �滻һ��
      Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
      if not (crfReplaceAll in Flags) then
      begin
        Result := Result + NewStr;
        Break;
      end;
    end
    else // ����ƥ���Ҫ���£�δ����ƥ�䣬�����滻
    begin
      Result := Result + Copy(NewStr, 1, Offset - 1) + OldPattern; // ע������� OldePattern�������滻
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

{$ELSE}

function CnStringReplaceW(const S, OldPattern, NewPattern: WideString;
  Flags: TCnReplaceFlags): WideString;
var
  SearchStr, Patt, NewStr: WideString;
  Offset, TailOffset: Integer;
  IsWhole: Boolean;
begin
  if crfIgnoreCase in Flags then
  begin
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
  end
  else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;

  NewStr := S;
  Result := '';

  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    IsWhole := True;
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end
    else if crfWholeWord in Flags then
    begin
      // �ҵ����Ӵ�����Ҫ����ƥ�䣬����������жϣ���������
      // ��ͷ��ͷ�Ƿָ���������β��β�Ƿָ������������
      if (Offset > 1) and not IsSepCharW(SearchStr[Offset - 1]) then
        IsWhole := False
      else
      begin
        TailOffset := Offset + Length(Patt); // ָ��ƥ����һ���ַ�
        if (TailOffset <= Length(SearchStr)) and not IsSepCharW(SearchStr[TailOffset]) then
          IsWhole := False;
      end;

      // �õ����Ƿ�����ƥ��Ľ���
    end;

    if not (crfWholeWord in Flags) or IsWhole then // ��ͨƥ�������ƥ����
    begin
      // �滻һ��
      Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
      if not (crfReplaceAll in Flags) then
      begin
        Result := Result + NewStr;
        Break;
      end;
    end
    else // ����ƥ���Ҫ���£�δ����ƥ�䣬�����滻
    begin
      Result := Result + Copy(NewStr, 1, Offset - 1) + OldPattern; // ע������� OldePattern�������滻
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

{$ENDIF}

function CnPosEx(const SubStr, S: string; CaseSensitive: Boolean; WholeWords:
  Boolean; StartCount: Integer): Integer;
var
  P: PChar;
  I, Count, Len, SubLen: Integer;
  StrUpper, SubUpper: string;
begin
  Result := 0;
  if (SubStr = '') or (S = '') or (StartCount < 1) then
    Exit;

  Len := Length(S);
  SubLen := Length(SubStr);
  if SubLen > Len then
    Exit;

  if not CaseSensitive then
  begin
    StrUpper := UpperCase(S);
    SubUpper := UpperCase(SubStr);
    P := PChar(StrUpper);
  end
  else
    P := PChar(S);

  Count := 0;
  for I := 1 to Len - SubLen + 1 do
  begin
    if (CaseSensitive and (P^ = SubStr[1]) and
      (CompareMem(P, PChar(SubStr), SubLen * SizeOf(Char))))
      or
      (not CaseSensitive and (P^ = SubUpper[1]) and
      (CompareMem(P, PChar(SubUpper), SubLen * SizeOf(Char)))) then
    begin
      if WholeWords then
      begin
        // ����Ƿ�����ƥ��
        if ((I = 1) or IsSepChar((P - 1)^)) and
           ((I + SubLen - 1 >= Len) or IsSepChar((P + SubLen)^)) then
        begin
          Inc(Count);
          if Count = StartCount then
          begin
            Result := I;
            Exit;
          end;
        end;
      end
      else
      begin
        Inc(Count);
        if Count = StartCount then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
    Inc(P);
  end;
end;

{$WARNINGS ON}

{ TCnStringBuilder }

constructor TCnStringBuilder.Create;
begin
  inherited;
  if not FModeIsFromOut then // �ⲿδָ��ʱ�Զ�����ģʽ
  begin
{$IFDEF UNICODE}
    FAnsiMode := False;
{$ELSE}
    FAnsiMode := True;
{$ENDIF}
  end
  else
    FAnsiMode := FOutMode;

  if FAnsiMode then
    FMaxCharCapacity := MaxInt
  else
    FMaxCharCapacity := MaxInt div 2;

  CharCapacity := STRING_BUILDER_DEFAULT_CAPACITY;
  FCharLength := 0;
end;

function TCnStringBuilder.Append(const Value: string): TCnStringBuilder;
begin
{$IFDEF UNICODE}
   if FAnsiMode then
     Result := AppendAnsi(AnsiString(Value))
   else
     Result := AppendString(Value);
{$ELSE}
   if FAnsiMode then
     Result := AppendString(Value)
   else
     Result := AppendWide(WideString(Value));
{$ENDIF}
end;

{$IFDEF UNICODE}

function TCnStringBuilder.AppendAnsi(const Value: AnsiString): TCnStringBuilder;
var
  Delta, OL: Integer;
begin
  Delta := Length(Value);
  if Delta <> 0 then
  begin
    OL := CharLength;
    CharLength := CharLength + Delta;
    if CharLength > CharCapacity then
      ExpandCharCapacity;
    Move(Pointer(Value)^, (PAnsiChar(Pointer(FAnsiData)) + OL)^, Delta * SizeOf(AnsiChar));
  end;
  Result := Self;
end;

{$ELSE}

function TCnStringBuilder.AppendWide(const Value: WideString): TCnStringBuilder;
var
  Delta, OL: Integer;
begin
  Delta := Length(Value);
  if Delta <> 0 then
  begin
    OL := CharLength;
    CharLength := CharLength + Delta;
    if CharLength > CharCapacity then
      ExpandCharCapacity;
    Move(Pointer(Value)^, (PWideChar(Pointer(FWideData)) + OL)^, Delta * SizeOf(WideChar));
  end;
  Result := Self;
end;

{$ENDIF}

constructor TCnStringBuilder.Create(IsAnsi: Boolean);
begin
  FModeIsFromOut := True;
  FOutMode := IsAnsi;     // ������ⲿָ�� AnsiMode
  Create;
end;

destructor TCnStringBuilder.Destroy;
begin
  inherited;

end;

procedure TCnStringBuilder.ExpandCharCapacity;
var
  NC: Integer;
begin
  NC := (CharCapacity * 3) div 2;
  if CharLength > NC then
    NC := CharLength * 2;
  if NC > FMaxCharCapacity then
    NC := FMaxCharCapacity;
  if NC < 0 then
    NC := CharLength;

  CharCapacity := NC;
end;

function TCnStringBuilder.GetCharCapacity: Integer;
begin
{$IFDEF UNICODE}
   if FAnsiMode then
     Result := Length(FAnsiData)
   else
     Result := Length(FData);
{$ELSE}
   if FAnsiMode then
     Result := Length(FData)
   else
     Result := Length(FWideData);
{$ENDIF}
end;

procedure TCnStringBuilder.SetCharCapacity(const Value: Integer);
begin
  if (Value < FCharLength) or (Value > FMaxCharCapacity) then
    raise ERangeError.CreateResFmt(@SListCapacityError, [Value]);

{$IFDEF UNICODE}
   if FAnsiMode then  
     SetLength(FAnsiData, Value)   // FAnsiData
   else
     SetLength(FData, Value);      // FData
{$ELSE}
   if FAnsiMode then
     SetLength(FData, Value)       // FData
   else
     SetLength(FWideData, Value);  // FWideData
{$ENDIF}
end;

procedure TCnStringBuilder.SetCharLength(const Value: Integer);
var
  OL: Integer;
begin
  if (Value < 0) or (Value > FMaxCharCapacity) then
    raise ERangeError.CreateResFmt(@SListCapacityError, [Value]);

  OL := FCharLength;
  try
    FCharLength := Value;
    if FCharLength > CharCapacity then
      ExpandCharCapacity;
  except
    on E: EOutOfMemory do
    begin
      FCharLength := OL;
      raise;
    end;
  end;
end;

function TCnStringBuilder.AppendString(const Value: string): TCnStringBuilder;
var
  Delta, OL: Integer;
begin
  Delta := Length(Value);
  if Delta <> 0 then
  begin
    OL := CharLength;
    FCharLength := CharLength + Delta;
    if CharLength > CharCapacity then
      ExpandCharCapacity;

    Move(Pointer(Value)^, (PChar(Pointer(FData)) + OL)^, Delta * SizeOf(Char));
  end;
  Result := Self;
end;

function TCnStringBuilder.ToString: string;
begin
  if FCharLength = CharCapacity then
    Result := FData
  else
    Result := Copy(FData, 1, FCharLength);
end;

function TCnStringBuilder.ToAnsiString: AnsiString;
begin
{$IFDEF UNICODE}
  if FAnsiMode then // Unicode ����������� Ansi ģʽ���� FAnsiData������ת��
  begin
    if FCharLength = CharCapacity then
      Result := FAnsiData
    else
      Result := Copy(FAnsiData, 1, FCharLength);
  end
  else  // Unicode ����������Ƿ� Ansi ģʽ���� FData������ AnsiString ת��
  begin
    if FCharLength = CharCapacity then
      Result := AnsiString(FData)
    else
      Result := AnsiString(Copy(FData, 1, FCharLength));
  end;
{$ELSE}
  Result := ToString; // �� Unicode �����µ��� ToString
{$ENDIF}
end;

function TCnStringBuilder.ToWideString: WideString;
begin
{$IFNDEF UNICODE}
  if FAnsiMode then // �� Unicode ����������� Ansi ģʽ���� FData������ WideString ת��
  begin
    if FCharLength = CharCapacity then
      Result := WideString(FData)
    else
      Result := WideString(Copy(FData, 1, FCharLength));
  end
  else // �� Unicode ����������Ƿ� Ansi ģʽ���� FWideData������ת��
  begin
    if FCharLength = CharCapacity then
      Result := FWideData
    else
      Result := Copy(FWideData, 1, FCharLength);
  end;
{$ELSE}
  Result := ToString; // Unicode �����µ��� ToString
{$ENDIF}
end;

function TCnStringBuilder.Append(Value: Integer): TCnStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TCnStringBuilder.Append(Value: SmallInt): TCnStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TCnStringBuilder.Append(Value: TObject): TCnStringBuilder;
begin
{$IFDEF OBJECT_HAS_TOSTRING}
  Result := Append(Value.ToString);
{$ELSE}
  Result := Append(IntToHex(TCnNativeInt(Value), 2));
{$ENDIF}
end;

function TCnStringBuilder.Append(Value: Int64): TCnStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TCnStringBuilder.Append(Value: Double): TCnStringBuilder;
begin
  Result := Append(FloatToStr(Value));
end;

function TCnStringBuilder.Append(Value: Byte): TCnStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TCnStringBuilder.Append(Value: Boolean): TCnStringBuilder;
begin
  if Value then
    Result := Append('True')
  else
    Result := Append('False');
end;

function TCnStringBuilder.AppendCurrency(Value: Currency): TCnStringBuilder;
begin
  Result := Append(CurrToStr(Value));
end;

function TCnStringBuilder.AppendChar(Value: Char): TCnStringBuilder;
var
  S: string;
begin
  SetLength(S, 1);
  Move(Value, S[1], SizeOf(Char));
  Result := Append(S);
end;

function TCnStringBuilder.Append(Value: ShortInt): TCnStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function TCnStringBuilder.Append(Value: Char;
  RepeatCount: Integer): TCnStringBuilder;
begin
  Result := Append(StringOfChar(Value, RepeatCount));
end;

function TCnStringBuilder.Append(Value: PAnsiChar): TCnStringBuilder;
begin
  Result := Append(string(Value));
end;

function TCnStringBuilder.Append(const Value: string; StartIndex,
  Count: Integer): TCnStringBuilder;
begin
  Result := Append(Copy(Value, StartIndex, Count));
end;

function TCnStringBuilder.Append(Value: Cardinal): TCnStringBuilder;
begin
  Result := Append(UInt32ToStr(Value));
end;

{$IFDEF SUPPORT_UINT64}

function TCnStringBuilder.Append(Value: UInt64): TCnStringBuilder;
begin
  Result := Append(UInt64ToStr(Value));
end;

{$ENDIF}

function TCnStringBuilder.Append(Value: Single): TCnStringBuilder;
begin
  Result := Append(FloatToStr(Value));
end;

function TCnStringBuilder.Append(Value: Word): TCnStringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

procedure TCnStringBuilder.Clear;
begin
  CharLength := 0;
  CharCapacity := STRING_BUILDER_DEFAULT_CAPACITY;
end;

function TCnStringBuilder.AppendLine: TCnStringBuilder;
begin
  Result := Append(SLineBreak);
end;

function TCnStringBuilder.AppendLine(const Value: string): TCnStringBuilder;
begin
  Result := Append(Value + SLineBreak);
end;

function TCnStringBuilder.Append(const AFormat: string;
  const Args: array of const): TCnStringBuilder;
begin
  Result := Append(Format(AFormat, Args));
end;

function TCnStringBuilder.AppendAnsiChar(Value: AnsiChar): TCnStringBuilder;
var
  S: AnsiString;
begin
  SetLength(S, 1);
  Move(Value, S[1], SizeOf(AnsiChar));
{$IFDEF UNICODE}
  Result := AppendAnsi(S);
{$ELSE}
  Result := Append(S); // Unicode �� S תΪ string ���ܻ���ʺ�
{$ENDIF}
end;

function TCnStringBuilder.AppendWideChar(Value: WideChar): TCnStringBuilder;
var
  S: WideString;
begin
  SetLength(S, 1);
  Move(Value, S[1], SizeOf(WideChar));
{$IFDEF UNICODE}
  Result := Append(S);
{$ELSE}
  Result := AppendWide(S);
{$ENDIF}
end;

end.
