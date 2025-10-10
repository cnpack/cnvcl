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

unit CnWideStrings;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�WideStrings ��Ԫ��֧�� Win32/64 �� Posix
* ��Ԫ���ߣ�CnPack ������
* ��    ע���õ�Ԫʵ���˼򻯵� TCnWideStringList ���벿�� Unicode �ַ���������
*           �Լ���չ�� UTF-8 �� UTF-16 �ı���뺯����֧�� UTF-16 �е����ֽ��ַ��� UTF8-MB4
*
*           ���⣬����Ԫ�ڴ��� Ansi �ַ����� Utf16 ���ַ�����תʱ���漰
*           һ�����ַ����ֽ���������ռ����п���ռ��ʾ��ȱ�����������
*           ������������ͬ�����ų����������������ǰ�߲��ܵ�ͬ���ʴ���Ҫ���� ByteLength �� DisplayLength
*           ��ȡ�ַ����ֽ��������� IDE ��Ϊ�޹أ������� ByteLength ϵ�к���
*           ��������ռ����п����ռ��ʾ��ȱ�������Ҫ��� IDE ��Ϊ�йأ��� IDE �汾�йأ�
*           ��������� DisplayLength ϵ�к�����������ͬ�ط����벻ͬ�� Calculator ���м���
*
*           ���䣺Lazarus IDE �б���ʱʹ�� LConvEncoding ����ת�����ƺ����׵����
*
* ����ƽ̨��WinXP SP3 + Delphi 5.0
* ���ݲ��ԣ�
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.08.06 V1.3
*               Ansi ת��Ϊ Utf8 ֧�� FPC
*           2024.08.01 V1.3
*               �������ָ�����ַ�����ʾ��ȼ���ص��������㲿���Զ����������
*               ���������ֳ� Ansi �� ByteLength �� DisplayLength ϵ�к���
*               �ж���ʾ��ȡ�����еȣ���Ҫ�� DisplayLength ϵ�к���
*               �� IDE ������Ҫ�󣬻��ô��붨�ƻ��� Calculator
*           2022.11.25 V1.2
*               �� CnGB18030 �а��ƹ������� Unicode ������
*           2022.11.10 V1.1
*               UTF-8 �������֧�� UTF8-MB4 �� UTF-16 �е����ֽ��ַ�
*           2010.01.16 by ZhouJingyu
*               ��ʼ���ύ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// {$DEFINE UTF16_BE}

// Delphi Ĭ�� UTF16-LE�����Ҫ���� UTF16-BE �ַ�������Ҫ���� UTF16_BE

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF} SysUtils, Classes, CnNative
  {$IFDEF LAZARUS}, LConvEncoding {$ENDIF};

const
  CN_INVALID_CODEPOINT = $FFFFFFFF;
  {* �Ƿ������ֵ}

  CN_ALTERNATIVE_CHAR  = '?';
  {* ����ת����������ʱ��Ĭ���滻�ַ�}

type
{$IFDEF UNICODE}
  TCnWideString = string;
{$ELSE}
  TCnWideString = WideString;
{$ENDIF}

  TCnCodePoint = type Cardinal;
  {* �ַ���ֵ�����߽���㣬�����ڱ��ı��뷽ʽ}

  TCn2CharRec = packed record
  {* ˫�ֽ��ַ��ṹ}
    P1: AnsiChar;
    P2: AnsiChar;
  end;
  PCn2CharRec = ^TCn2CharRec;

  TCn4CharRec = packed record
  {* ���ֽ��ַ��ṹ}
    P1: AnsiChar;
    P2: AnsiChar;
    P3: AnsiChar;
    P4: AnsiChar;
  end;
  PCn4CharRec = ^TCn4CharRec;

{ TCnWideStringList }

  TCnWideListFormat = (wlfAnsi, wlfUtf8, wlfUnicode);
  {* �����뱣��ʱ֧�ֵ����ֱ��룬Ansi��Utf8��Utf16}

  TCnWideStringList = class;
  TCnWideStringListSortCompare = function(List: TCnWideStringList; Index1, Index2: Integer): Integer;

  PCnWideStringItem = ^TCnWideStringItem;
  TCnWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  TCnWideStringList = class(TPersistent)
  {* WideString ��� TStringList ʵ�֣�Load/Save ʱ�б���Ĵ���}
  private
    FList: TList;
    FUseSingleLF: Boolean;
    FLoadFormat: TCnWideListFormat;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure SetValue(const Name, Value: WideString);
    procedure QuickSort(L, R: Integer; SCompare: TCnWideStringListSortCompare);
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; const Value: TObject);
  protected
    function Get(Index: Integer): WideString; virtual;
    function GetCount: Integer; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: WideString): Integer; virtual;
    procedure AddStrings(Strings: TCnWideStringList); virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual; 
    procedure Exchange(Index1, Index2: Integer); virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer;
    procedure Insert(Index: Integer; const S: WideString); virtual;
    procedure LoadFromFile(const FileName: WideString); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: WideString; AFormat: TCnWideListFormat = wlfUnicode); virtual;
    procedure SaveToStream(Stream: TStream; AFormat: TCnWideListFormat = wlfUnicode); virtual;
    procedure CustomSort(Compare: TCnWideStringListSortCompare); virtual;
    procedure Sort; virtual;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;

    property UseSingleLF: Boolean read FUseSingleLF write FUseSingleLF;
    {* ���� GetTextStr ʱʹ�õĻ����Ƿ��ǵ��� #10 �����ǳ���� #13#10}
    property LoadFormat: TCnWideListFormat read FLoadFormat;
    {* LoadFromStream ʱʶ����ĸ�ʽ}
  end;

  TCnWideCharDisplayWideLengthCalculator = function(AWChar: WideChar): Boolean;
  {* ��Կ��ַ�����ʾ��ȼ���ص��������ͣ���ͬ�� Delphi IDE �༭������Ҫ��ͬ��ʵ��}

function CnUtf8EncodeWideString(const S: TCnWideString): AnsiString;
{* �� WideString ���� UTF-8 ���벢�����ݷŵ� AnsiString �з��أ����� Ansi ת�����ⶪ�ַ���
   ֧�����ֽ� UTF-16 �ַ��� UTF8-MB4��

   ������
     const S: WideString/UnicodeString    - ��ת���Ŀ��ַ���

   ����ֵ��AnsiString                     - ���� UTF-8 �ַ���
}

function CnUtf8DecodeToWideString(const S: AnsiString): TCnWideString;
{* �������� UTF-8 ����� AnsiString ���� UTF-8 ����õ� WideString������ Ansi ת�����ⶪ�ַ���
   ֧�����ֽ� UTF-16 �ַ��� UTF8-MB4��

   ������
     const S: AnsiString                  - ��ת���� UTF-8 �ַ���

   ����ֵ��WideString/UnicodeString       - ���صĿ��ַ���
}

function GetUtf16HighByte(Rec: PCn2CharRec): Byte; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* �õ�һ�� UTF-16 ˫�ֽ��ַ��ĸ�λ�ֽ�ֵ��

   ������
     Rec: PCn2CharRec                     - ����ȡ��˫�ֽ��ַ��ṹָ��

   ����ֵ��Byte                           - ���ظ�λ�ֽ�ֵ
}

function GetUtf16LowByte(Rec: PCn2CharRec): Byte; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* �õ�һ�� UTF-16 ˫�ֽ��ַ��ĵ�λ�ֽ�ֵ��

   ������
     Rec: PCn2CharRec                     - ����ȡ��˫�ֽ��ַ��ṹָ��

   ����ֵ��Byte                           - ���ص�λ�ֽ�ֵ
}

procedure SetUtf16HighByte(B: Byte; Rec: PCn2CharRec); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ����һ�� UTF-16 ˫�ֽ��ַ��ĸ�λ�ֽ�ֵ��

   ������
     B: Byte                              - �����õĸ�λ�ֽ�ֵ
     Rec: PCn2CharRec                     - �����õ�˫�ֽ��ַ��ṹָ��

   ����ֵ�����ޣ�
}

procedure SetUtf16LowByte(B: Byte; Rec: PCn2CharRec); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ����һ�� UTF-16 ˫�ֽ��ַ��ĵ�λ�ֽ�ֵ

   ������
     B: Byte                              - �����õĵ�λ�ֽ�ֵ
     Rec: PCn2CharRec                     - �����õ�˫�ֽ��ַ��ṹָ��

   ����ֵ�����ޣ�
}

function GetCharLengthFromUtf8(Utf8Str: PAnsiChar): Integer;
{* ����һ UTF-8�������� UTF8-MB4���ַ������ַ�����

   ������
     Utf8Str: PAnsiChar                   - ������� UTF-8 �ַ�����ַ

   ����ֵ��Integer                        - ���ظ��ַ������ַ���
}

function GetCharLengthFromUtf16(Utf16Str: PWideChar): Integer;
{* ����һ UTF-16�����ܻ�� Unicode ��չƽ��������ֽ��ַ����ַ������ַ�����

   ������
     Utf16Str: PWideChar                  - ������� UTF-16 �ַ�����ַ

   ����ֵ��Integer                        - ���ظ��ַ������ַ���
}

function GetByteWidthFromUtf8(Utf8Str: PAnsiChar): Integer;
{* ����һ UTF-8�������� UTF8-MB4���ַ����ĵ�ǰ�ַ�ռ�����ֽڡ�

   ������
     Utf8Str: PAnsiChar                   - ������� UTF-8 �ַ�����ַ

   ����ֵ��Integer                        - ���ظ��ַ������ֽ���
}

function GetByteWidthFromUtf16(Utf16Str: PWideChar): Integer;
{* ����һ UTF-16�����ܻ�� Unicode ��չƽ��������ֽ��ַ����ַ����ĵ�ǰ�ַ�ռ�����ֽڡ�

   ������
     Utf16Str: PWideChar                  - ������� UTF-16 �ַ�����ַ

   ����ֵ��Integer                        - ���ظ��ַ������ֽ���
}

function GetCodePointFromUtf16Char(Utf16Str: PWideChar): TCnCodePoint;
{* ����һ�� UTF-16 �ַ��ı���ֵ��Ҳ�д���λ�ã���ע�� Utf16Str ����ָ��һ��˫�ֽ��ַ���Ҳ����ָ��һ�����ֽ��ַ�

   ������
     Utf16Str: PWideChar                  - ������� UTF-16 �ַ���ַ

   ����ֵ��TCnCodePoint                   - ���ظ��ַ��ı���ֵ
}

function GetCodePointFromUtf164Char(PtrTo4Char: Pointer): TCnCodePoint;
{* ����һ�����ֽ� UTF-16 �ַ��ı���ֵ��Ҳ�д���λ�ã���

   ������
     PtrTo4Char: Pointer                  - ����������ֽ� UTF-16 �ַ���ַ

   ����ֵ��TCnCodePoint                   - ���ظ��ַ��ı���ֵ
}

function GetUtf16CharFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
{* ����һ�� Unicode ����ֵ�Ķ��ֽڻ����ֽڱ�ʾ����� PtrToChars ָ���λ�ò�Ϊ�գ�
   �򽫽������ PtrToChars ��ָ�Ķ��ֽڻ����ֽ�����������Ƿ����򷵻� 1 ���� PtrToChars Ϊ #0#0��
   �������� CP ���� $FFFF ʱ�뱣֤ PtrToChars ��ָ�������������ֽڣ���֮���ֽڼ��ɡ�
   ���� 1 �� 2���ֱ��ʾ������Ƕ��ֽڻ����ֽڡ�

   ������
     CP: TCnCodePoint                     - ������� Unicode ����ֵ
     PtrToChars: Pointer                  - ����� nil�������ת����Ľ��

   ����ֵ��Integer                        - ���� 1 ������ַ�ռ���ֽڣ����� 2 �������ֽ�
}

// =============================================================================
//
// ���º����漰���ַ����� UTF-8 ת��ʱ�ļ��㣬�߼��ȽϹ̶�
//
// =============================================================================

function CalcUtf8LengthFromWideString(Text: PWideChar): Integer;
{* ������ַ����� UTF-8 ���ȣ����� Utf8Encode ��ȡ Length����������ʵ��ת����

   ������
     Text: PWideChar                      - ������Ŀ��ַ�����ַ

   ����ֵ��Integer                        - ���� UTF-8 �ֽڳ���
}

function CalcUtf8LengthFromWideChar(AChar: WideChar): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ����һ�� WideChar ת���� UTF-8 ����ַ����ȡ�

   ������
     AChar: WideChar                      - ������Ŀ��ַ�

   ����ֵ��Integer                        - ���� UTF-8 �ֽڳ���
}

function CalcUtf8LengthFromWideStringOffset(Text: PWideChar; WideOffset: Integer): Integer;
{* ���� Unicode ���ַ����� 1 �� WideOffset ���Ӵ��� UTF-8 ���ȣ�WideOffset �� 1 ��ʼ����� WideOffset �� 0 �򷵻� 0��
   ���� Copy(1, WideOffset) ����Ӵ�ת UTF-8 ȡ Length����������ʵ��ת����

   ������
     Text: PWideChar                      - ������Ŀ��ַ�����ַ
     WideOffset: Integer                  - �Կ��ַ�Ϊ��λ��ƫ����

   ����ֵ��Integer                        - ���ظÿ��ַ����� 1 �� WideOffset ���Ӵ��� UTF-8 ����
}

function CalcUtf8LengthFromWideStringAnsiOffset(Text: PWideChar; AnsiOffset: Integer): Integer;
{* ���� Unicode ���ַ���ת�� Ansi ��� 1 �� AnsiOffset ���Ӵ��� UTF-8 ���ȣ�AnsiOffset �� 1 ��ʼ����� AnsiOffset �� 0 �򷵻� 0��
   ����ת Ansi �� Copy(1, AnsiOffset) ����Ӵ�ת�� Unicode ���ַ�����ת UTF-8 ȡ Length����������ʵ��ת����

   ������
     Text: PWideChar                      - ������Ŀ��ַ�����ַ
     AnsiOffset: Integer                  - �� Ansi �ַ�Ϊ��λ��ƫ����

   ����ֵ��Integer                        - ���ظÿ��ַ���ת�� Ansi ��� 1 �� AnsiOffset ���Ӵ��� UTF-8 ����
}

function CalcUtf8LengthFromUtf8HeadChar(AChar: AnsiChar): Integer;
{* ����һ�� UTF-8 ǰ���ַ���������ַ����ȡ�

   ������
     AChar: AnsiChar                      - ������� UTF-8 �ַ�

   ����ֵ��Integer                        - �����ַ�����
}

function CalcUtf8StringLengthFromWideOffset(Utf8Text: PAnsiChar; WideOffset: Integer): Integer;
{* ���� UTF-8 �ַ���ת���� WideSting ��ָ�� Wide �Ӵ����ȶ�Ӧ�� UTF-8 �ַ������ȣ�WideOffset �� 1 ��ʼ��
   ����ת WideString �� Copy(1, WideOffset) ��ת�� UTF-8 ��ȡ Length�������� UTF-8/WideString ��ת���Ա������ı������⡣

   ������
     Utf8Text: PAnsiChar                  - ������� UTF-8 �ַ�����ַ
     WideOffset: Integer                  - �Կ��ַ�Ϊ��λ��ƫ����

   ����ֵ��Integer                        - ���ظ� UTF-8 �ַ���ת���� WideSting ��ָ���� 1 �� WideOffset �Ӵ�����Ӧ�� UTF-8 �ַ�������
}

// =============================================================================
//
// ���º����漰���ַ����� Ansi ת��ʱ���ֽ���������ռ����п�/��ռ��ʾ��ȱ����ȵļ���
//
// =============================================================================

function WideCharIsWideLength(const AWChar: WideChar): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* �����ж�һ�� Unicode ���ַ��Ƿ�ռ�����ַ���ȣ�Ĭ�ϵļ�ªʵ�֣��� IDE �汾����Ϊ�޹ء�
   �����º����е� TCnWideCharDisplayWideLengthCalculator ������Ĭ��ʵ�֡�

   ������
     const AWChar: WideChar               - ���жϵĿ��ַ�

   ����ֵ��Boolean                        - �����Ƿ�ռ�����ַ����
}

function CalcAnsiByteLengthFromWideString(Text: PWideChar): Integer;
{* ���� Unicode ���ַ����� Ansi �ֽڳ��ȣ�����ת Ansi ��� Length��������ת Ansi���Է�ֹ��Ӣ��ƽ̨�¶��ַ���
   ���� $FF �� UTF-16 �ַ����� 2 �ֽڣ�����Ϊ 1 �ֽڡ�

   ������
     Text: PWideChar                      - ������Ŀ��ַ�����ַ

   ����ֵ��Integer                        - ����ת����� Ansi �ַ�������
}

function CalcAnsiDisplayLengthFromWideString(Text: PWideChar;
  Calculator: TCnWideCharDisplayWideLengthCalculator = nil): Integer;
{* ���� Unicode ���ַ����� Ansi ��ʾ���ȣ�����ת Ansi �����ʾ Length��������ת Ansi���Է�ֹ��Ӣ��ƽ̨�¶��ַ���
   �Դ���� Calculator ��������ʾ���ַ���ȣ�����ʱ����Ĭ���жϡ�

   ������
     Text: PWideChar                                      - ������Ŀ��ַ�����ַ
     Calculator: TCnWideCharDisplayWideLengthCalculator   - ��Կ��ַ�����ʾ��ȼ���ص���������ͬ�� Delphi IDE �༭�����в�ͬ���������

   ����ֵ��Integer                                        - ����ת����� Ansi �ַ�����ʾ����
}

function CalcAnsiByteLengthFromWideStringOffset(Text: PWideChar; WideOffset: Integer): Integer;
{* ���� Unicode ���ַ����� 1 �� WideOffset ���Ӵ��� Ansi �ֽڳ��ȣ�WideOffset �� 1 ��ʼ��
   ���� Copy(1, WideOffset) ����Ӵ�ת Ansi �ֽ�ȡ Length��������ʵ��ת Ansi���Է�ֹ��Ӣ��ƽ̨�¶��ַ���
   ���� $FF �� UTF-16 �ַ����� 2 �ֽڣ�����Ϊ 1 �ֽڡ�

   ������
     Text: PWideChar                      - ������Ŀ��ַ�����ַ
     WideOffset: Integer                  - �Կ��ַ�Ϊ��λ��ƫ����

   ����ֵ��Integer                        - ���ظÿ��ַ����� 1 �� WideOffset �Ӵ��� Ansi �ֽڳ���
}

function CalcAnsiDisplayLengthFromWideStringOffset(Text: PWideChar; WideOffset: Integer;
  Calculator: TCnWideCharDisplayWideLengthCalculator = nil): Integer;
{* ���� Unicode ���ַ����� 1 �� WideOffset ���Ӵ��� Ansi ��ʾ���ȣ�WideOffset �� 1 ��ʼ��
   ���� Copy(1, WideOffset) ����Ӵ�ת Ansi ȡ Length��������ʵ��ת Ansi���Է�ֹ��Ӣ��ƽ̨�¶��ַ�
   �Դ���� Calculator ��������ʾ���ַ���ȣ�����ʱ����Ĭ���жϡ�

   ������
     Text: PWideChar                                      - ������Ŀ��ַ�����ַ
     WideOffset: Integer                                  - ����ַ��Ŀ�ȼ���ص�����ͬ�� Delphi IDE �༭�����в�ͬ���������
     Calculator: TCnWideCharDisplayWideLengthCalculator   - ��Կ��ַ�����ʾ��ȼ���ص���������ͬ�� Delphi IDE �༭�����в�ͬ���������

   ����ֵ��Integer                                        - ���ظÿ��ַ����� 1 �� WideOffset �Ӵ��� Ansi ��ʾ����
}

function CalcWideStringByteLengthFromAnsiOffset(Text: PWideChar; AnsiOffset: Integer;
  AllowExceedEnd: Boolean = False): Integer;
{* ���� Unicode ���ַ���ָ�� Ansi �Ӵ����ȶ�Ӧ�� Unicode �Ӵ����ֽڳ��ȣ�AnsiOffset �� 1 ��ʼ��
   ��������ת Ansi ��� Copy(1, AnsiOffset) ��ת���� Unicode ��ȡ Length�������� Ansi/Unicode ��ת���Է�ֹ��Ӣ��ƽ̨�¶��ַ�
   ע�� Ansi ��� Copy ���ܻ����˫�ֽ��ַ���
   AllowExceedEnd Ϊ False ʱ�����㵽 #0 �����ֹ�������� #0��Ϊ True ʱ���Բ��ո�ʽ���㡣
   ���� $FF �� UTF-16 �ַ����� 2 �ֽڣ�����Ϊ 1 �ֽڡ�

   ������
     Text: PWideChar                      - ������Ŀ��ַ�����ַ
     AnsiOffset: Integer                  - �Ե��ֽ��ַ�Ϊ��λ��ƫ����
     AllowExceedEnd: Boolean              - �Ƿ����� #0 ʱ��ֹ

   ����ֵ��Integer                        - ���ظÿ��ַ���ת��Ϊ Ansi ��� 1 �� AnsiOffset �Ӵ����ȶ�Ӧ�� Unicode �ַ������ֽڳ���
}

function CalcWideStringDisplayLengthFromAnsiOffset(Text: PWideChar; AnsiOffset: Integer;
  AllowExceedEnd: Boolean = False; Calculator: TCnWideCharDisplayWideLengthCalculator = nil): Integer;
{* ���� Unicode ���ַ���ָ�� Ansi �Ӵ����ȶ�Ӧ�� Unicode �Ӵ����ȣ�AnsiOffset �� 1 ��ʼ��
   ������ʾת Ansi ��� Copy(1, AnsiOffset) ��ת���� Unicode ��ȡ Length�������� Ansi/Unicode ��ת���Է�ֹ��Ӣ��ƽ̨�¶��ַ�
   ע�� Ansi ��� Copy ���ܻ����˫�ֽ��ַ���
   AllowExceedEnd Ϊ False ʱ�����㵽 #0 �����ֹ�������� #0��Ϊ True ʱ���Բ��ո�ʽ����
   �Դ���� Calculator ��������ʾ���ַ���ȣ�����ʱ����Ĭ���жϡ�

   ������
     Text: PWideChar                                      - ������Ŀ��ַ�����ַ
     AnsiOffset: Integer                                  - �Ե��ֽ��ַ�Ϊ��λ��ƫ����
     AllowExceedEnd: Boolean                              - �Ƿ����� #0 ʱ��ֹ
     Calculator: TCnWideCharDisplayWideLengthCalculator   - ��Կ��ַ�����ʾ��ȼ���ص���������ͬ�� Delphi IDE �༭�����в�ͬ���������

   ����ֵ��Integer                                        - ���ظÿ��ַ���ת��Ϊ Ansi ��� 1 �� AnsiOffset �Ӵ����ȶ�Ӧ�� Unicode �ַ�������ʾ����
}

function CalcUtf8LengthFromWideStringAnsiDisplayOffset(Text: PWideChar; AnsiDisplayOffset: Integer;
  Calculator: TCnWideCharDisplayWideLengthCalculator = nil): Integer;
{* ���� Unicode ���ַ���ת����ʾ��ص� Ansi ��� 1 �� AnsiOffset ���Ӵ��� UTF-8 ���ȣ�AnsiDisplayOffset �� 1 ��ʼ����� AnsiDisplayOffset �� 0 �򷵻� 0��
   ����ת��ʾ��ص� Ansi �� Copy(1, AnsiDisplayOffset) ����Ӵ�ת�� Unicode ���ַ�����ת UTF-8 ȡ Length����������ʵ��ת����

   ������
     Text: PWideChar                                      - ������Ŀ��ַ�����ַ
     AnsiDisplayOffset: Integer                           - ����ʾ��ص� Ansi �ַ�Ϊ��λ��ƫ����
     Calculator: TCnWideCharDisplayWideLengthCalculator   - ��Կ��ַ�����ʾ��ȼ���ص���������ͬ�� Delphi IDE �༭�����в�ͬ���������

   ����ֵ��Integer                                        - ���ظÿ��ַ���ת����ʾ��ص� Ansi ��� 1 �� AnsiDisplayOffset ���Ӵ��� UTF-8 ����
}

function ConvertUtf16ToAlterDisplayAnsi(WideText: PWideChar; AlterChar: AnsiChar = ' ';
  Calculator: TCnWideCharDisplayWideLengthCalculator = nil): AnsiString;
{* �ֶ������ַ���ת������ʾ�õ� Ansi�������еĿ��ַ��� Calculator ���ж��滻��һ�������� AlterChar��
   ����ʱ����Ĭ���жϡ����ڴ�Ӣ�Ļ����µ��ַ���ʾ��ȼ��㣬����֧�����ֽ��ַ���

   ������
     WideText: PWideChar                                  - ��ת���Ŀ��ַ�����ַ
     AlterChar: AnsiChar                                  - �滻�ַ�
     Calculator: TCnWideCharDisplayWideLengthCalculator   - ��Կ��ַ�����ʾ��ȼ���ص���������ͬ�� Delphi IDE �༭�����в�ͬ���������

   ����ֵ��AnsiString                                     - ����ת������ַ���
}

function ConvertUtf8ToAlterDisplayAnsi(Utf8Text: PAnsiChar; AlterChar: AnsiChar = ' ';
  Calculator: TCnWideCharDisplayWideLengthCalculator = nil): AnsiString;
{* �ֶ��� UTF-8 �ַ���ת������ʾ�õ� Ansi�������еĿ��ַ��� Calculator ���ж��滻��һ�������� AlterChar��
   ����ʱ����Ĭ���жϡ����ڴ�Ӣ�Ļ����µ��ַ���ʾ��ȼ��㣬����֧�����ֽ��ַ���

   ������
     Utf8Text: PAnsiChar                                  - ��ת���� UTF-8 �ַ�����ַ
     AlterChar: AnsiChar                                  - �滻�ַ�
     Calculator: TCnWideCharDisplayWideLengthCalculator   - ��Կ��ַ�����ʾ��ȼ���ص���������ͬ�� Delphi IDE �༭�����в�ͬ���������

   ����ֵ��AnsiString                                     - ����ת������ַ���
}

function CnUtf8ToAnsi(const Text: AnsiString): AnsiString;
{* Ansi ���ת�� UTF-8 �� Ansi �ַ������Խ�� Unicode �汾�� Utf8ToAnsi �� UnicodeString �����⡣

   ������
     const Text: AnsiString               - ��ת���� UTF-8 �ַ���

   ����ֵ��AnsiString                     - ����ת������ַ���
}

function CnUtf8ToAnsi2(const Text: string): string;
{* Ansi ���ת�� UTF-8 �� string���Խ�� Unicode �汾�� Utf8ToAnsi �� UnicodeString �����⡣

   ������
     const Text: string                   - ��ת���� UTF-8 �ַ���

   ����ֵ��string                         - ����ת������ַ���
}

function CnAnsiToUtf8(const Text: AnsiString): AnsiString;
{* Ansi ���ת�� Ansi �ַ����� UTF-8 �ַ������Խ�� Unicode �汾�� AnsiToUtf8 �� UnicodeString �����⡣

   ������
     const Text: AnsiString               - ��ת���� Ansi �ַ���

   ����ֵ��AnsiString                     - ����ת����� UTF-8 �ַ���
}

function CnAnsiToUtf82(const Text: string): string;
{* Ansi ���ת�� Ansi �ַ����� UTF-8 �ַ������Խ�� Unicode �汾�� AnsiToUtf8 �� UnicodeString �����⡣

   ������
     const Text: string                   - ��ת���� Ansi �ַ���

   ����ֵ��string                         - ����ת����� UTF-8 �ַ���
}

implementation

const
  SLineBreak = #13#10;
  SLineBreakLF = #10;

  CN_UTF16_4CHAR_PREFIX1_LOW  = $D8;
  CN_UTF16_4CHAR_PREFIX1_HIGH = $DC;
  CN_UTF16_4CHAR_PREFIX2_LOW  = $DC;
  CN_UTF16_4CHAR_PREFIX2_HIGH = $E0;

  CN_UTF16_4CHAR_HIGH_MASK    = $3;
  CN_UTF16_4CHAR_SPLIT_MASK   = $3FF;

  CN_UTF16_EXT_BASE           = $10000;

resourcestring
  SCnErrorInvalidUtf8CharLength = 'More than UTF8-MB4 NOT Support.';

{ TCnWideStringList }

function WideCompareText(const S1, S2: WideString): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(S1),
    Length(S1), PWideChar(S2), Length(S2)) - 2;
{$ELSE}
  Result := WideCompareStr(S1, S2);
{$ENDIF}
end;

function TCnWideStringList.Add(const S: WideString): Integer;
begin
  Result := Count;
  Insert(Count, S);
end;

function TCnWideStringList.AddObject(const S: WideString;
  AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TCnWideStringList.AddStrings(Strings: TCnWideStringList);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TCnWideStringList.Assign(Source: TPersistent);
begin
  if Source is TCnWideStringList then
  begin
    Clear;
    AddStrings(TCnWideStringList(Source));
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TCnWideStringList.Clear;
var
  I: Integer;
  P: PCnWideStringItem;
begin
  for I := 0 to Count - 1 do
  begin
    P := PCnWideStringItem(FList[I]);
    Dispose(P);
  end;
  FList.Clear;
end;

constructor TCnWideStringList.Create;
begin
  inherited;
  FList := TList.Create;
  FLoadFormat := wlfUnicode;
end;

procedure TCnWideStringList.CustomSort(Compare: TCnWideStringListSortCompare);
begin
  if Count > 1 then
    QuickSort(0, Count - 1, Compare);
end;

procedure TCnWideStringList.Delete(Index: Integer);
var
  P: PCnWideStringItem;
begin
  P := PCnWideStringItem(FList[Index]);
  FList.Delete(Index);
  Dispose(P);
end;

destructor TCnWideStringList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TCnWideStringList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TCnWideStringList.Get(Index: Integer): WideString;
begin
  Result := PCnWideStringItem(FList[Index])^.FString;
end;

function TCnWideStringList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCnWideStringList.GetName(Index: Integer): WideString;
var
  P: Integer;
begin
  Result := Get(Index);
  P := Pos('=', Result);
  if P <> 0 then
    SetLength(Result, P - 1) else
    SetLength(Result, 0);
end;

function TCnWideStringList.GetObject(Index: Integer): TObject;
begin
  Result := PCnWideStringItem(FList[Index])^.FObject;
end;

function TCnWideStringList.GetTextStr: WideString;
var
  I, L, Size, C: Integer;
  P: PwideChar;
  S, LB: WideString;
begin
  C := GetCount;
  Size := 0;

  if FUseSingleLF then
    LB := SLineBreakLF
  else
    LB := SLineBreak;

  for I := 0 to C - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to C - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(WideChar));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L * SizeOf(WideChar));
      Inc(P, L);
    end;
  end;
end;

function TCnWideStringList.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TCnWideStringList.IndexOf(const S: WideString): Integer;
begin
  for Result := 0 to GetCount - 1 do
  begin
    if WideCompareText(Get(Result), S) = 0 then
      Exit;
  end;
  Result := -1;
end;

function TCnWideStringList.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P <> 0) and (WideCompareText(Copy(S, 1, P - 1), Name) = 0) then
      Exit;
  end;
  Result := -1;
end;

procedure TCnWideStringList.Insert(Index: Integer; const S: WideString);
var
  P: PCnWideStringItem;
begin
  New(P);
  P^.FString := S;
  FList.Insert(Index, P);
end;

procedure TCnWideStringList.LoadFromFile(const FileName: WideString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnWideStringList.LoadFromStream(Stream: TStream);
var
  Size, Len: Integer;
  S: WideString;
  HeaderStr, SA: AnsiString;
begin
  Size := Stream.Size - Stream.Position;
  if Size >= 3 then
  begin
    SetLength(HeaderStr, 3);
    Stream.Read(Pointer(HeaderStr)^, 3);
    if HeaderStr = #$EF#$BB#$BF then // UTF-8 BOM
    begin
      SetLength(SA, Size - 3);
      Stream.Read(Pointer(SA)^, Size - 3);
{$IFDEF MSWINDOWS}
      Len := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(SA), -1, nil, 0);
      SetLength(S, Len);
      MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(SA), -1, PWideChar(S), Len);
{$ELSE}
  {$IFDEF FPC}
      S := CnUtf8DecodeToWideString(SA);
  {$ELSE}
      S := UTF8ToWideString(SA);
  {$ENDIF}
{$ENDIF}
      SetTextStr(S);

      FLoadFormat := wlfUtf8;
      Exit;
    end;
    Stream.Position := Stream.Position - 3;  
  end;

  if Size >= 2 then
  begin
    SetLength(HeaderStr, 2);
    Stream.Read(Pointer(HeaderStr)^, 2);
    if HeaderStr = #$FF#$FE then // UTF-16 BOM
    begin
      SetLength(S, (Size - 2) div SizeOf(WideChar));
      Stream.Read(Pointer(S)^, (Size - 2) div SizeOf(WideChar) * SizeOf(WideChar));
      SetTextStr(S);

      FLoadFormat := wlfUnicode;
      Exit;
    end;
    Stream.Position := Stream.Position - 2;  
  end;
      
  SetString(SA, nil, Size);
  Stream.Read(Pointer(SA)^, Size);
  SetTextStr({$IFDEF UNICODE}string{$ENDIF}(SA));
  FLoadFormat := wlfAnsi;
end;

procedure TCnWideStringList.Put(Index: Integer; const S: WideString);
var
  P: PCnWideStringItem;
begin
  P := PCnWideStringItem(FList[Index]);
  P^.FString := S;
end;

procedure TCnWideStringList.PutObject(Index: Integer; const Value: TObject);
begin
  PCnWideStringItem(FList[Index])^.FObject := Value;
end;

procedure TCnWideStringList.QuickSort(L, R: Integer;
  SCompare: TCnWideStringListSortCompare);
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
        Exchange(I, J);
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

procedure TCnWideStringList.SaveToFile(const FileName: WideString; AFormat: TCnWideListFormat);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, AFormat);
  finally
    Stream.Free;
  end;
end;

procedure TCnWideStringList.SaveToStream(Stream: TStream; AFormat: TCnWideListFormat);
var
  S: WideString;
  HeaderStr, SA: AnsiString;
  Len: Integer;
begin
  S := GetTextStr;
  if AFormat = wlfAnsi then
  begin
    SA := AnsiString(S);
    Stream.WriteBuffer(Pointer(SA)^, Length(SA) * SizeOf(AnsiChar));
  end
  else if AFormat = wlfUtf8 then
  begin
    HeaderStr := #$EF#$BB#$BF;
    Stream.WriteBuffer(Pointer(HeaderStr)^, Length(HeaderStr) * SizeOf(AnsiChar));
{$IFDEF MSWINDOWS}
    Len := WideCharToMultiByte(CP_UTF8, 0, PWideChar(S), -1, nil, 0, nil, nil);
    SetLength(SA, Len);
    WideCharToMultiByte(CP_UTF8, 0, PWideChar(S), -1, PAnsiChar(SA), Len, nil, nil);
{$ELSE}
    SA := UTF8Encode(S);
{$ENDIF}
    Stream.WriteBuffer(Pointer(SA)^, Length(SA) * SizeOf(AnsiChar) - 1);
  end
  else if AFormat = wlfUnicode then
  begin
    HeaderStr := #$FF#$FE;
    Stream.WriteBuffer(Pointer(HeaderStr)^, Length(HeaderStr) * SizeOf(AnsiChar));
    Stream.WriteBuffer(Pointer(S)^, Length(S) * SizeOf(WideChar));
  end;
end;

procedure TCnWideStringList.SetTextStr(const Value: WideString);
var
  P, Start: PWideChar;
  S: WideString;
begin
  Clear;
  P := Pointer(Value);
  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
      while not (Ord(P^) in [0, 10, 13]) do Inc(P);
      SetString(S, Start, P - Start);
      Add(S);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
end;

procedure TCnWideStringList.SetValue(const Name, Value: WideString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

function StringListCompareStrings(List: TCnWideStringList; Index1, Index2: Integer): Integer;
begin
  Result := WideCompareText(PCnWideStringItem(List.FList[Index1])^.FString,
    PCnWideStringItem(List.FList[Index2])^.FString);
end;

procedure TCnWideStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

// D5 ��û������ UTF-8/Ansi ת���������ҵͰ汾��ʹ��Ҳ��֧�� UTF8-MB4�����д�����Ʒ
// Ϊ�����߼��������SourceChars ��˫�ֽڿ��ַ���������
function InternalUnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal;
  Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  I, Cnt: Cardinal;
  C: Cardinal;
begin
  Result := 0;
  if Source = nil then
    Exit;

  Cnt := 0;
  I := 0;
  if Dest <> nil then
  begin
    while (I < SourceChars) and (Cnt < MaxDestBytes) do
    begin
      if (SourceChars - I >= 2) and (GetByteWidthFromUtf16(@(Source[I])) = 4) then
      begin
        // ���ַ������ֽڣ�Ҫ�������
        C := GetCodePointFromUtf164Char(PAnsiChar(@(Source[I])));
        Inc(I, 2); // �������� WideChar
      end
      else
      begin
        C := Cardinal(Source[I]);
        Inc(I); // ����һ�� WideChar
      end;

      if C <= $7F then
      begin
        Dest[Cnt] := AnsiChar(C);
        Inc(Cnt);
      end
      else if C > $FFFF then
      begin
        if Cnt + 4 > MaxDestBytes then
          Break;

        Dest[Cnt] := AnsiChar($F0 or (C shr 18));
        Dest[Cnt + 1] := AnsiChar($80 or ((C shr 12) and $3F));
        Dest[Cnt + 2] := AnsiChar($80 or ((C shr 6) and $3F));
        Dest[Cnt + 3] := AnsiChar($80 or (C and $3F));
        Inc(Cnt, 4);
      end
      else if C > $7FF then
      begin
        if Cnt + 3 > MaxDestBytes then
          Break;
        Dest[Cnt] := AnsiChar($E0 or (C shr 12));
        Dest[Cnt + 1] := AnsiChar($80 or ((C shr 6) and $3F));
        Dest[Cnt + 2] := AnsiChar($80 or (C and $3F));
        Inc(Cnt, 3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if Cnt + 2 > MaxDestBytes then
          Break;
        Dest[Cnt] := AnsiChar($C0 or (C shr 6));
        Dest[Cnt + 1] := AnsiChar($80 or (C and $3F));
        Inc(Cnt, 2);
      end;
    end;

    if Cnt >= MaxDestBytes then
      Cnt := MaxDestBytes - 1;
    Dest[Cnt] := #0;
  end
  else
  begin
    while I < SourceChars do
    begin
      if (SourceChars - I >= 2) and (GetByteWidthFromUtf16(@(Source[I])) = 4) then
      begin
        // ���ַ������ֽڣ�Ҫ�������
        C := GetCodePointFromUtf164Char(PAnsiChar(@(Source[I])));
        Inc(I, 2); // �������� WideChar
      end
      else
      begin
        C := Cardinal(Source[I]);
        Inc(I);
      end;

      if C > $7F then
      begin
        if C > $7FF then
        begin
          if C > $FFFF then
            Inc(Cnt);
          Inc(Cnt);
        end;
        Inc(Cnt);
      end;
      Inc(Cnt);
    end;
  end;
  Result := Cnt + 1;
end;

function InternalUtf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal;
  Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
var
  K: Integer;
  I, Cnt: Cardinal;
  C: Byte;
  WC: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;

  Result := Cardinal(-1);
  Cnt := 0;
  I := 0;
  if Dest <> nil then
  begin
    while (I < SourceBytes) and (Cnt < MaxDestChars) do
    begin
      WC := Cardinal(Source[I]);
      Inc(I);

      if (WC and $80) <> 0 then
      begin
        if I >= SourceBytes then                // ������
          Exit;

        if (WC and $F0) = $F0 then              // ���ֽڣ�δ�޶�����λ������ 0�������������ٲ��������ַ���ƴ���ַ�ֵ����������ֽڵ� UTF-16 ����
        begin
          if SourceBytes - I < 3 then           // �������ֽ�������˳�
            Exit;

          // WC �ǵ�һ���ֽڣ�ȡ����λ��δ�޶�����λ������ 0�����������ֽڸ�ȡ����λ���õ����
          WC := ((WC and $7) shl 18) + ((Cardinal(Source[I]) and $3F) shl 12)
            + ((Cardinal(Source[I + 1]) and $3F) shl 6) + (Cardinal(Source[I + 2]) and $3F);

          // ����������� UTF-16 �ַ��������� Cnt
          K := GetUtf16CharFromCodePoint(WC, @(Dest[Cnt]));
          if K = 2 then // ���������ֽ��ַ����Ȳ���һ�� WideChar����һ���� if �󲽽�
            Inc(Cnt);
          Inc(I, 3);
        end
        else
        begin
          WC := WC and $3F;
          if (WC and $20) <> 0 then
          begin
            C := Byte(Source[I]);
            Inc(I);
            if (C and $C0) <> $80 then           // malformed trail byte or out of range char
              Exit;
            if I >= SourceBytes then             // incomplete multibyte char
              Exit;
            WC := (WC shl 6) or (C and $3F);
          end;
          C := Byte(Source[I]);
          Inc(I);
          if (C and $C0) <> $80 then             // malformed trail byte
            Exit;

          Dest[Cnt] := WideChar((WC shl 6) or (C and $3F));
        end;
      end
      else
        Dest[Cnt] := WideChar(WC);
      Inc(Cnt);
    end;
    if Cnt >= MaxDestChars then Cnt := MaxDestChars - 1;
    Dest[Cnt] := #0;
  end
  else
  begin
    while (I < SourceBytes) do
    begin
      C := Byte(Source[I]);
      Inc(I);

      if (C and $80) <> 0 then                  // ���λΪ 1�����ٶ��ֽ�
      begin
        if I >= SourceBytes then                // incomplete multibyte char
          Exit;

        C := C and $3F;                         // ���µ�һ���ֽڵĵ���λ��ǰ��λ�Ѿ����� 11 ��
        if (C and $20) <> 0 then                // ����� 1110�����ʾ���������ֽ�
        begin
          if (C and $10) <> 0 then              // ����� 11110�����ʾ�������ֽ�
          begin
            C := Byte(Source[I]);               // �����ĸ��еĵڶ����ֽ�
            Inc(I);
            if (C and $C0) <> $80 then          // ���ֽ������λ���� 10
              Exit;                             // malformed trail byte or out of range char
            if I >= SourceBytes then
              Exit;                             // incomplete multibyte char

            Inc(Cnt);                           // ���ֽڵ� UTF8��Ӧ��Ӧ UTF-16 �е����� WideChar����������һ
          end;

          C := Byte(Source[I]);                 // ���ĸ��еĵ������ֽڣ��������еĵڶ����ֽ�
          Inc(I);
          if (C and $C0) <> $80 then            // ���ֽ������λ���� 10�������˳�
            Exit;
          if I >= SourceBytes then
            Exit;                               // incomplete multibyte char
        end;

        C := Byte(Source[I]);                   // ���ĸ��еĵ��ĸ��ֽڣ��������еĵ������ֽڣ�������еĵڶ����ֽ�
        Inc(I);
        if (C and $C0) <> $80 then              // ���ֽ������λ���� 10�������˳�
          Exit;                                 // malformed trail byte
      end;

      Inc(Cnt);
    end;
  end;
  Result := Cnt + 1;
end;

// �� WideString ���� UTF-8 ����õ� AnsiString������ Ansi ת�����ⶪ�ַ�
function CnUtf8EncodeWideString(const S: TCnWideString): AnsiString;
var
  L: Integer;
  Temp: AnsiString;
begin
  Result := '';
  if S = '' then
    Exit;
  SetLength(Temp, Length(S) * 4); // һ��˫�ֽ��ַ���� 4 �� UTF-8 �ַ�

  L := InternalUnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

// �� AnsiString �� UTF-8 ����õ� WideString������ Ansi ת�����ⶪ�ַ�
function CnUtf8DecodeToWideString(const S: AnsiString): TCnWideString;
var
  L: Integer;
begin
  Result := '';
  if S = '' then
    Exit;
  SetLength(Result, Length(S));

  L := InternalUtf8ToUnicode(PWideChar(Result), Length(Result) + 1, PAnsiChar(S), Length(S));
  if L > 0 then
    SetLength(Result, L - 1)
  else
    Result := '';
end;

function GetUtf16HighByte(Rec: PCn2CharRec): Byte;
begin
{$IFDEF UTF16_BE}
  Result := Byte(Rec^.P1);
{$ELSE}
  Result := Byte(Rec^.P2); // UTF16-LE �ĸߵ�λ���û�
{$ENDIF}
end;

function GetUtf16LowByte(Rec: PCn2CharRec): Byte;
begin
{$IFDEF UTF16_BE}
  Result := Byte(Rec^.P2);
{$ELSE}
  Result := Byte(Rec^.P1); // UTF16-LE �ĸߵ�λ���û�
{$ENDIF}
end;

procedure SetUtf16HighByte(B: Byte; Rec: PCn2CharRec);
begin
{$IFDEF UTF16_BE}
  Rec^.P1 := AnsiChar(B);
{$ELSE}
  Rec^.P2 := AnsiChar(B); // UTF16-LE �ĸߵ�λ���û�
{$ENDIF}
end;

procedure SetUtf16LowByte(B: Byte; Rec: PCn2CharRec);
begin
{$IFDEF UTF16_BE}
  Rec^.P2 := AnsiChar(B);
{$ELSE}
  Rec^.P1 := AnsiChar(B); // UTF16-LE �ĸߵ�λ���û�
{$ENDIF}
end;

function GetCharLengthFromUtf8(Utf8Str: PAnsiChar): Integer;
var
  L: Integer;
begin
  Result := 0;
  while Utf8Str^ <> #0 do
  begin
    L := GetByteWidthFromUtf8(Utf8Str);
    Inc(Utf8Str, L);
    Inc(Result);
  end;
end;

function GetCharLengthFromUtf16(Utf16Str: PWideChar): Integer;
var
  L: Integer;
begin
  Result := 0;
  while Utf16Str^ <> #0 do
  begin
    L := GetByteWidthFromUtf16(Utf16Str);
    Utf16Str := PWideChar(TCnIntAddress(Utf16Str) + L);
    Inc(Result);
  end;
end;

function GetByteWidthFromUtf8(Utf8Str: PAnsiChar): Integer;
var
  B: Byte;
begin
  B := Byte(Utf8Str^);
  if B >= $FC then        // 6 �� 1��1 �� 0���Ȳ������߻�� 1 �����
    Result := 6
  else if B >= $F8 then   // 5 �� 1��1 �� 0
    Result := 5
  else if B >= $F0 then   // 4 �� 1��1 �� 0
    Result := 4
  else if B >= $E0 then   // 3 �� 1��1 �� 0
    Result := 3
  else if B >= $B0 then   // 2 �� 1��1 �� 0
    Result := 2
  else                    // ����
    Result := 1;
end;

function GetByteWidthFromUtf16(Utf16Str: PWideChar): Integer;
var
  P: PCn2CharRec;
  B1, B2: Byte;
begin
  Result := 2;

  P := PCn2CharRec(Utf16Str);
  B1 := GetUtf16HighByte(P);

  if (B1 >= CN_UTF16_4CHAR_PREFIX1_LOW) and (B1 < CN_UTF16_4CHAR_PREFIX1_HIGH) then
  begin
    // ����������ֽ��ַ�ƴһ�飬��ֵ�� $D800 �� $DBFF ֮�䣬Ҳ���Ǹ�˫�ֽڵĸ�λ�ֽ��� [$D8, $DC) ������
    Inc(P);
    B2 := GetUtf16HighByte(P);

    // ��ô�����ں�����������ֽ��ַ�Ӧ���� $DC00 �� $DFFF ֮�䣬
    if (B2 >= CN_UTF16_4CHAR_PREFIX2_LOW) and (B2 < CN_UTF16_4CHAR_PREFIX2_HIGH) then
      Result := 4;

    // ���ĸ��ֽ����һ�����ֽ� Unicode �ַ��������Ǹ�ֵ�ı���ֵ
  end;
end;

function GetCodePointFromUtf16Char(Utf16Str: PWideChar): TCnCodePoint;
var
  R: Word;
  C2: PCn2CharRec;
begin
  if GetByteWidthFromUtf16(Utf16Str) = 4 then // ���ֽ��ַ�
    Result := GetCodePointFromUtf164Char(PAnsiChar(Utf16Str))
  else  // ��ͨ˫�ֽ��ַ�
  begin
    C2 := PCn2CharRec(Utf16Str);
    R := Byte(C2^.P1) shl 8 + Byte(C2^.P2);       // ˫�ֽ��ַ���ֵ������Ǳ���ֵ

{$IFDEF UTF16_BE}
    Result := TCnCodePoint(R);
{$ELSE}
    Result := TCnCodePoint(UInt16ToBigEndian(R)); // UTF16-LE Ҫ����ֵ
{$ENDIF}
  end;
end;

function GetCodePointFromUtf164Char(PtrTo4Char: Pointer): TCnCodePoint;
var
  TH, TL: Word;
  C2: PCn2CharRec;
begin
  C2 := PCn2CharRec(PtrTo4Char);

  // ��һ���ֽڣ�ȥ����λ�� 110110���ڶ����ֽ����ţ��� 2 + 8 = 10 λ
  TH := (GetUtf16HighByte(C2) and CN_UTF16_4CHAR_HIGH_MASK) shl 8 + GetUtf16LowByte(C2);
  Inc(C2);

  // �������ֽڣ�ȥ����λ�� 110111�����ĸ��ֽ����ţ��� 2 + 8 = 10 λ
  TL := (GetUtf16HighByte(C2) and CN_UTF16_4CHAR_HIGH_MASK) shl 8 + GetUtf16LowByte(C2);

  // �� 10 λƴ�� 10 λ
  Result := TH shl 10 + TL + CN_UTF16_EXT_BASE;
  // ����ȥ $10000 ���ֵ��ǰ 10 λӳ�䵽 $D800 �� $DBFF ֮�䣬�� 10 λӳ�䵽 $DC00 �� $DFFF ֮��
end;

function GetUtf16CharFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
var
  C2: PCn2CharRec;
  L, H: Byte;
  LW, HW: Word;
begin
  if CP = CN_INVALID_CODEPOINT then
  begin
    if PtrToChars <> nil then
    begin
      C2 := PCn2CharRec(PtrToChars);
      SetUtf16LowByte(0, C2);
      SetUtf16HighByte(0, C2);
    end;
    Result := 1;
    Exit;
  end;

  if CP >= CN_UTF16_EXT_BASE then
  begin
    if PtrToChars <> nil then
    begin
      CP := CP - CN_UTF16_EXT_BASE;
      // ����� 10 λ��ǰ���ֽڣ������ 10 λ�ź����ֽ�

      LW := CP and CN_UTF16_4CHAR_SPLIT_MASK;          // �� 10 λ�����������ֽ�
      HW := (CP shr 10) and CN_UTF16_4CHAR_SPLIT_MASK; // �� 10 λ����һ�����ֽ�

      L := HW and $FF;
      H := (HW shr 8) and CN_UTF16_4CHAR_HIGH_MASK;
      H := H or CN_UTF16_4CHAR_PREFIX1_LOW;              // 1101 1000
      C2 := PCn2CharRec(PtrToChars);

      SetUtf16LowByte(L, C2);
      SetUtf16HighByte(H, C2);

      L := LW and $FF;
      H := (LW shr 8) and CN_UTF16_4CHAR_HIGH_MASK;
      H := H or CN_UTF16_4CHAR_PREFIX1_HIGH;              // 1101 1100
      Inc(C2);

      SetUtf16LowByte(L, C2);
      SetUtf16HighByte(H, C2);
    end;
    Result := 2;
  end
  else
  begin
    if PtrToChars <> nil then
    begin
      C2 := PCn2CharRec(PtrToChars);
      SetUtf16LowByte(Byte(CP and $00FF), C2);
      SetUtf16HighByte(Byte(CP shr 8), C2);
    end;
    Result := 1;
  end;
end;

// ������ַ����� UTF-8 ���ȣ����� Utf8Encode ��ȡ Length������ʵ��ת��
function CalcUtf8LengthFromWideString(Text: PWideChar): Integer;
begin
  Result := 0;
  if Text = nil then
    Exit;

  while Text^ <> #0 do
  begin
    Inc(Result, CalcUtf8LengthFromWideChar(Text^));
    Inc(Text);
  end;
end;

// ����һ�� WideChar ת���� UTF-8 ����ַ�����
function CalcUtf8LengthFromWideChar(AChar: WideChar): Integer;
var
  V: Cardinal;
begin
  V := Ord(AChar);
  if V <= $7F then
    Result := 1
  else if V <= $7FF then
    Result := 2
  else if V <= $FFFF then
    Result := 3
  else if V <= $10FFFF then
    Result := 4
  else
    Result := 0;
end;

// ���� Unicode ���ַ����� 1 �� WideOffset ���Ӵ��� UTF-8 ���ȣ�WideOffset �� 1 ��ʼ
function CalcUtf8LengthFromWideStringOffset(Text: PWideChar; WideOffset: Integer): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text <> nil) and (WideOffset > 0) then
  begin
    Idx := 0;
    while (Text^ <> #0) and (Idx < WideOffset) do // Idx 0 ��ʼ��WideOffset 1 ��ʼ�������� <
    begin
      Inc(Result, CalcUtf8LengthFromWideChar(Text^));
      Inc(Text);
      Inc(Idx);
    end;
  end;
end;

// ���� Unicode ���ַ���ת�� Ansi ��� 1 �� AnsiOffset ���Ӵ��� UTF-8 ���ȣ�AnsiOffset �� 1 ��ʼ
function CalcUtf8LengthFromWideStringAnsiOffset(Text: PWideChar; AnsiOffset: Integer): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text <> nil) and (AnsiOffset > 0) then
  begin
    Idx := 0;
    while (Text^ <> #0) and (Idx < AnsiOffset) do // Idx 0 ��ʼ��AnsiOffset 1 ��ʼ�������� <
    begin
      Inc(Result, CalcUtf8LengthFromWideChar(Text^));
      Inc(Text);
      if Ord(Text^) > $FF then // ���� $FF ��ת�� Ansi ��Ȼռ���ֽ�
        Inc(Idx, 2)
      else
        Inc(Idx);
    end;
  end;
end;

// ����һ�� UTF-8 ǰ���ַ���������ַ�����
function CalcUtf8LengthFromUtf8HeadChar(AChar: AnsiChar): Integer;
var
  B: Byte;
begin
  B := Ord(AChar);
  if B and $80 = 0 then  // 0xxx xxxx
    Result := 1
  else if B and $E0 = $C0 then // 110x xxxx 10xxxxxx
    Result := 2
  else if B and $F0 = $E0 then // 1110 xxxx 10xxxxxx 10xxxxxx
    Result := 3
  else if B and $F8 = $F0 then // 1111 0xxx 10xxxxxx 10xxxxxx 10xxxxxx
    Result := 4
  else
    raise Exception.Create(SCnErrorInvalidUtf8CharLength);
end;

// ���� UTF-8 �ַ���ת���� WideSting ��ָ�� Wide �Ӵ����ȶ�Ӧ�� UTF-8 �ַ������ȣ�WideOffset �� 1 ��ʼ��
// ����ת WideString �� Copy(1, WideOffset) ��ת�� UTF-8 ��ȡ Length�������� UTF-8/WideString ��ת���Ա������ı�������
function CalcUtf8StringLengthFromWideOffset(Utf8Text: PAnsiChar;
  WideOffset: Integer): Integer;
var
  Utf8Len, WideIdx: Integer;
begin
  Result := 0;
  if (Utf8Text = nil) or (WideOffset <= 0) then
    Exit;

  WideIdx := 0;
  while (Utf8Text^ <> #0) and (WideIdx < WideOffset) do
  begin
    Utf8Len := CalcUtf8LengthFromUtf8HeadChar(Utf8Text^);
    Inc(Result, Utf8Len);

    case Utf8Len of
      1:
        begin
          Inc(WideIdx);
          Inc(Utf8Text);
        end;
      2:
        begin
          Inc(WideIdx);
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
        end;
      3:
        begin
          Inc(WideIdx);
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
        end;
      4: // UTF8-MB4
        begin
          Inc(WideIdx);
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
        end;
    else
      Exit;
    end;
  end;
end;

// �����ж�һ�� Unicode ���ַ��Ƿ�ռ�����ַ���ȣ�Ĭ�ϵļ�ªʵ��
function WideCharIsWideLength(const AWChar: WideChar): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
const
  CN_UTF16_ANSI_WIDE_CHAR_SEP = $1100;
var
  C: Integer;
begin
  C := Ord(AWChar);
  Result := C > CN_UTF16_ANSI_WIDE_CHAR_SEP; // ������Ϊ�� $1100 ��� Utf16 �ַ����ƿ�Ȳ�ռ���ֽ�
end;

function CalcAnsiByteLengthFromWideString(Text: PWideChar): Integer;
begin
  Result := 0;
  if Text = nil then
    Exit;

  while Text^ <> #0 do
  begin
    if Ord(Text^) > $FF then
      Inc(Result, SizeOf(WideChar))
    else
      Inc(Result, SizeOf(AnsiChar));
    Inc(Text);
  end;
end;

// ���� Unicode ���ַ����� Ansi ���ȣ�����ת Ansi ��� Length��������ת Ansi���Է�ֹ��Ӣ��ƽ̨�¶��ַ�
function CalcAnsiDisplayLengthFromWideString(Text: PWideChar;
  Calculator: TCnWideCharDisplayWideLengthCalculator): Integer;
begin
  Result := 0;
  if Text = nil then
    Exit;

  if not Assigned(Calculator) then
    Calculator := @WideCharIsWideLength;

  while Text^ <> #0 do
  begin
    if Calculator(Text^) then
      Inc(Result, SizeOf(WideChar))
    else
      Inc(Result, SizeOf(AnsiChar));
    Inc(Text);
  end;
end;

function CalcAnsiByteLengthFromWideStringOffset(Text: PWideChar; WideOffset: Integer): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text = nil) or (WideOffset <= 0) then
    Exit;

  Idx := 0;
  while (Text^ <> #0) and (Idx < WideOffset) do // Idx 0 ��ʼ��WideOffset 1 ��ʼ�������� <
  begin
    if Ord(Text^) > $FF then
      Inc(Result, SizeOf(WideChar))
    else
      Inc(Result, SizeOf(AnsiChar));
    Inc(Text);
    Inc(Idx);
  end;
end;

// ���� Unicode ���ַ����� 1 �� WideOffset ���Ӵ��� Ansi ���ȣ�WideOffset �� 1 ��ʼ��
function CalcAnsiDisplayLengthFromWideStringOffset(Text: PWideChar; WideOffset: Integer;
  Calculator: TCnWideCharDisplayWideLengthCalculator): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text = nil) or (WideOffset <= 0) then
    Exit;

  Idx := 0;
  if not Assigned(Calculator) then
    Calculator := @WideCharIsWideLength;

  while (Text^ <> #0) and (Idx < WideOffset) do // Idx 0 ��ʼ��WideOffset 1 ��ʼ�������� <
  begin
    if Calculator(Text^) then
      Inc(Result, SizeOf(WideChar))
    else
      Inc(Result, SizeOf(AnsiChar));
    Inc(Text);
    Inc(Idx);
  end;
end;

function CalcWideStringByteLengthFromAnsiOffset(Text: PWideChar;
  AnsiOffset: Integer; AllowExceedEnd: Boolean): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text <> nil) and (AnsiOffset > 0) then
  begin
    Idx := 0;
    while (Text^ <> #0) and (Idx < AnsiOffset) do
    begin
      if Ord(Text^) > $FF then
        Inc(Idx, SizeOf(WideChar))
      else
        Inc(Idx, SizeOf(AnsiChar));
      Inc(Text);
      Inc(Result);
    end;

    if AllowExceedEnd and (Text^ = #0) and (Idx < AnsiOffset) then
      Inc(Result, AnsiOffset - Idx);
  end;
end;

function CalcWideStringDisplayLengthFromAnsiOffset(Text: PWideChar; AnsiOffset: Integer;
  AllowExceedEnd: Boolean; Calculator: TCnWideCharDisplayWideLengthCalculator): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text <> nil) and (AnsiOffset > 0) then
  begin
    Idx := 0;
    if not Assigned(Calculator) then
      Calculator := @WideCharIsWideLength;

    while (Text^ <> #0) and (Idx < AnsiOffset) do
    begin
      if Calculator(Text^) then
        Inc(Idx, SizeOf(WideChar))
      else
        Inc(Idx, SizeOf(AnsiChar));
      Inc(Text);
      Inc(Result);
    end;

    if AllowExceedEnd and (Text^ = #0) and (Idx < AnsiOffset) then
      Inc(Result, AnsiOffset - Idx);
  end;
end;

// ���� Unicode ���ַ���ת����ʾ��ص� Ansi ��� 1 �� AnsiOffset ���Ӵ��� UTF-8 ���ȣ�AnsiDisplayOffset �� 1 ��ʼ
function CalcUtf8LengthFromWideStringAnsiDisplayOffset(Text: PWideChar;
  AnsiDisplayOffset: Integer; Calculator: TCnWideCharDisplayWideLengthCalculator): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text <> nil) and (AnsiDisplayOffset > 0) then
  begin
    Idx := 0;
    if not Assigned(Calculator) then
      Calculator := @WideCharIsWideLength;

    while (Text^ <> #0) and (Idx < AnsiDisplayOffset) do // Idx 0 ��ʼ��AnsiDisplayOffset 1 ��ʼ�������� <
    begin
      Inc(Result, CalcUtf8LengthFromWideChar(Text^));
      Inc(Text);
      if Calculator(Text^) then
        Inc(Idx, SizeOf(WideChar))
      else
        Inc(Idx, SizeOf(AnsiChar));
    end;
  end;
end;

// �ֶ������ַ���ת���� Ansi�������еĿ��ַ����滻������ AlterChar�����ڴ�Ӣ�Ļ����µ��ַ���ȼ���
function ConvertUtf16ToAlterDisplayAnsi(WideText: PWideChar; AlterChar: AnsiChar;
  Calculator: TCnWideCharDisplayWideLengthCalculator): AnsiString;
var
  Len: Integer;
begin
  if WideText = nil then
  begin
    Result := '';
    Exit;
  end;

{$IFDEF UNICODE}
  Len := StrLen(WideText);
{$ELSE}
  Len := Length(WideString(WideText));
{$ENDIF}

  if Len = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Len * SizeOf(WideChar));

  if not Assigned(Calculator) then
    Calculator := @WideCharIsWideLength;

  Len := 0;
  while WideText^ <> #0 do
  begin
    if Calculator(WideText^) then
    begin
      Inc(Len);
      Result[Len] := AlterChar;
      Inc(Len);
      Result[Len] := AlterChar;
    end
    else
    begin
      Inc(Len);
      if Ord(WideText^) <= $FF then // Absolutely 'Single' Char
        Result[Len] := AnsiChar(WideText^)
      else                          // Extended 'Single' Char, Replace
        Result[Len] := AlterChar;
    end;
    Inc(WideText);
  end;
  SetLength(Result, Len);
end;

// �ֶ��� UTF-8 �ַ���ת���� Ansi�������еĿ��ַ����滻������ AlterChar�����ڴ�Ӣ�Ļ����µ��ַ���ȼ���
function ConvertUtf8ToAlterDisplayAnsi(Utf8Text: PAnsiChar; AlterChar: AnsiChar;
  Calculator: TCnWideCharDisplayWideLengthCalculator): AnsiString;
var
  I, J, Len, ByteCount: Integer;
  C: AnsiChar;
  W: Word;
  B, B1, B2: Byte;
begin
  Result := '';
  if Utf8Text = nil then
    Exit;

  Len := StrLen(Utf8Text);
  if Len = 0 then
    Exit;

  SetLength(Result, Len); // �����ԭ�ĳ�������ϳ�
  I := 0;
  J := 1;

  if not Assigned(Calculator) then
    Calculator := @WideCharIsWideLength;

  while I < Len do
  begin
    C := Utf8Text[I];
    B := Ord(C);
    W := 0;

    // ���� B ��ֵ�ó�����ַ�ռ����λ
    if B and $80 = 0 then  // 0xxx xxxx
      ByteCount := 1
    else if B and $E0 = $C0 then // 110x xxxx 10xxxxxx
      ByteCount := 2
    else if B and $F0 = $E0 then // 1110 xxxx 10xxxxxx 10xxxxxx
      ByteCount := 3
    else if B and $F8 = $F0 then // 1111 0xxx 10xxxxxx 10xxxxxx 10xxxxxx
      ByteCount := 4
    else
      raise Exception.Create('More than UTF32 NOT Support.');

    // �ټ������Ӧ�Ŀ��ֽ��ַ�
    case ByteCount of
      1:
      begin
        W := B and $7F;
      end;
      2:
      begin
        B1 := Ord(Utf8Text[I + 1]);
        W := ((B and $1F) shl 6) or (B1 and $3F);
      end;
      3:
      begin
        B1 := Ord(Utf8Text[I + 1]);
        B2 := Ord(Utf8Text[I + 2]);
        W := ((B and $0F) shl 12) or ((B1 and $3F) shl 6) or (B2 and $3F);
      end;
    end;

    if ByteCount = 4 then
    begin
      // ���ֽ� UTF8������תΪ�� WideChar��Ҳ�����ĸ��ַ�
      // TODO: ������ʾ���δ�أ��ܿ�������Ƨ�������������ַ�
      Result[J] := AlterChar;
      Inc(J);
      Result[J] := AlterChar;
      Inc(J);
      Result[J] := AlterChar;
      Inc(J);
      Result[J] := AlterChar;
      Inc(J);
    end
    else if Calculator(WideChar(W)) then // 3 �ֽ� UTF8���ж�ʵ�ʿ��
    begin
      Result[J] := AlterChar;
      Inc(J);
      Result[J] := AlterChar;
      Inc(J);
    end
    else
    begin
      if W <= 255 then
        Result[J] := AnsiChar(W)
      else
        Result[J] := AlterChar;
      Inc(J);
    end;

    Inc(I, ByteCount);
  end;

  SetLength(Result, J - 1); // Inc �� J ��׼������һ���ַ��ģ�û�˾ͼ�һ
end;

function CnUtf8ToAnsi(const Text: AnsiString): AnsiString;
begin
{$IFDEF FPC}
  {$IFDEF LAZARUS}
  Result := ConvertEncoding(Text, EncodingUTF8, EncodingAnsi);
  {$ELSE}
  Result := Utf8ToAnsi(Text);
  {$ENDIF}
{$ELSE}
{$IFDEF UNICODE}
  Result := AnsiString(UTF8ToUnicodeString(PAnsiChar(Text)));
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := Utf8ToAnsi(Text);
  {$ELSE}
  Result := AnsiString(CnUtf8DecodeToWideString(Text));
  {$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function CnUtf8ToAnsi2(const Text: string): string;
begin
{$IFDEF FPC}
  {$IFDEF LAZARUS}
  Result := ConvertEncoding(Text, EncodingUTF8, EncodingAnsi);
  {$ELSE}
  Result := Utf8ToAnsi(Text);
  {$ENDIF}
{$ELSE}
{$IFDEF UNICODE}
  Result := UTF8ToUnicodeString(PAnsiChar(AnsiString(Text)));
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := Utf8ToAnsi(Text);
  {$ELSE}
  Result := AnsiString(CnUtf8DecodeToWideString(Text));
  {$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function CnAnsiToUtf8(const Text: AnsiString): AnsiString;
begin
{$IFDEF FPC}
  {$IFDEF LAZARUS}
  Result := ConvertEncoding(Text, EncodingAnsi, EncodingUTF8);
  {$ELSE}
  Result := AnsiToUtf8(Text);
  {$ENDIF}
{$ELSE}
{$IFDEF UNICODE}
  Result := AnsiString(Utf8Encode(Text)); // ����ֵ���ɸ�Ϊ UTF8String ���ͣ�����˴�ת����Ч
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := AnsiToUtf8(Text);
  {$ELSE}
  Result := CnUtf8EncodeWideString(WideString(Text));
  {$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function CnAnsiToUtf82(const Text: string): string;
begin
{$IFDEF FPC}
  {$IFDEF LAZARUS}
  Result := ConvertEncoding(Text, EncodingAnsi, EncodingUTF8);
  {$ELSE}
  Result := AnsiToUtf8(Text);
  {$ENDIF}
{$ELSE}
{$IFDEF UNICODE}
  Result := string(Utf8Encode(Text));
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := AnsiToUtf8(Text);
  {$ELSE}
  Result := CnUtf8EncodeWideString(WideString(Text));
  {$ENDIF}
{$ENDIF}
{$ENDIF}
end;

end.
