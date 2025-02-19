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

unit CnStream;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ���չ�� Stream �൥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2009.04.03 V1.2
*               Xor �������������ܹ������ַ�����дλ�ÿ��ܲ��Ե�����
*           2003.03.02 V1.1
*               Xor ��������Ϊ�ַ���������ֵ
*           2002.10.28 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  {$IFDEF MSWINDOWS} Windows, {$ELSE} System.Types, {$ENDIF} SysUtils, Classes;

type

//==============================================================================
// ��չ�� TStream ��
//==============================================================================
   
{ TCnStream }

  ECnStreamError = class(EStreamError);
  {* �������쳣}
  ECnReadStreamError = class(ECnStreamError);
  {* �����ж�ȡ���ݳ���}
  ECnWriteStreamError = class(ECnStreamError);
  {* д���ݵ�������}

  TCnStreamDataType = (dtInteger, dtBool, dtDateTime, dtDouble, dtString, dtData);

  TCnStream = class (TStream)
  {* ��չ���������࣬�̳��� TStream��ͨ����װ����������ʵ�ֹ�����չ����Ҫ����
     ����һЩ�������������͡�}
  private
    FStream: TStream;
    FOwned: Boolean;
    function GetHandle: Integer;
    function GetMemory: Pointer;
  protected
    class procedure ReadError; reintroduce;
    class procedure WriteError; reintroduce;
    procedure WriteDataType(DataType: TCnStreamDataType);
    function ReadDataType: TCnStreamDataType;
    procedure DoRead(var Buffer; Count: Longint);
    procedure DoWrite(const Buffer; Count: Longint);

  {$IFDEF COMPILER7_UP}
    function GetSize: Int64; override;
  {$ENDIF}
    procedure SetSize(NewSize: Longint); override;
  {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); overload; override;
  {$ENDIF}

    property Owned: Boolean read FOwned;
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream; AOwned: Boolean = False); overload;
    constructor Create; overload;
    constructor Create(const FileName: string; Mode: Word); overload;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure BeginRead(Flag: Longint = -1);
    procedure EndRead(Flag: Longint = -1);
    procedure BeginWrite(Flag: Longint = -1);
    procedure EndWrite(Flag: Longint = -1);
    
    function ReadInteger: Longint;
    function ReadBool: Boolean;
    function ReadDateTime: TDateTime;
    function ReadFloat: Double;
    function ReadString: string;
    procedure ReadData(var Buffer; Count: Longint);

    procedure WriteInteger(Value: Longint);
    procedure WriteBool(Value: Boolean);
    procedure WriteDateTime(Value: TDateTime);
    procedure WriteFloat(Value: Double);
    procedure WriteString(Value: string);
    procedure WriteData(const Buffer; Count: Longint);

    property Memory: Pointer read GetMemory;
    property Handle: Integer read GetHandle;
  end;

//==============================================================================
// ���ܵ� TStream �������
//==============================================================================

{ TCnEncryptStream }

  TCnEncryptStream = class (TStream)
  {* ���ܵ� TStream ������֧࣬�����ݶ�дʱ���м��ܴ���}
  private
    FStream: TStream;
    FOwned: Boolean;
  protected
    procedure DeEncrypt(var Buffer; Count: Longint); virtual; abstract;
    {* ���ܷ��������󷽷���}
    procedure Encrypt(var Buffer; Count: Longint); virtual; abstract;
    {* ���ܷ��������󷽷���}

    procedure DoBeforeEncrypt(const Buffer; Count: Longint); virtual;
    procedure DoAfterEncrypt(const Buffer; Count: Longint); virtual;
    procedure DoBeforeDeEncrypt(const Buffer; Count: Longint); virtual;
    procedure DoAfterDeEncrypt(const Buffer; Count: Longint); virtual;

  {$IFDEF COMPILER7_UP}
    function GetSize: Int64; override;
  {$ENDIF}
    procedure SetSize(NewSize: Longint); override;
  {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); overload; override;
  {$ENDIF}
  
    property Owned: Boolean read FOwned;
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream; AOwned: Boolean = False);
    {* �๹������AStream ����Ϊ��Ҫ���м��ܴ��������AOwned ��ʾ�Ƿ�
       ���ͷż�����ʱͬʱ�ͷ� AStream��}
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): LongInt; override;
    function Seek(Offset: Longint; Origin: Word): LongInt; override;
    function Write(const Buffer; Count: Longint): LongInt; override;
  end;
  
//==============================================================================
// Xor ��ʽ���ܵ� TStream ��
//==============================================================================
   
{ TCnXorStream }

  TCnXorStream = class (TCnEncryptStream)
  {* Xor ���ܵ� TStream �֧࣬�����ݶ�дʱ���� Xor ���ܴ���}
  private
    FXorStr: AnsiString;
    FSeedPos: Integer;
  protected
    procedure DeEncrypt(var Buffer; Count: Longint); override;
    procedure Encrypt(var Buffer; Count: Longint); override;

    procedure DoBeforeEncrypt(const Buffer; Count: Longint); override;
    procedure DoAfterEncrypt(const Buffer; Count: Longint); override;
    procedure DoBeforeDeEncrypt(const Buffer; Count: Longint); override;
    procedure DoAfterDeEncrypt(const Buffer; Count: Longint); override;
  public
    constructor Create(AStream: TStream; const AXorStr: AnsiString;
      AOwned: Boolean = False);
    {* �๹����
     |<PRE>
       AStream: TStream         - ��Ҫ���м��ܴ������
       AXorStr: string          - ���ڼ��ܴ�����ַ���
       AOwned: Boolean          - �Ƿ����ͷż�����ʱͬʱ�ͷ� AStream
     |</PRE>}
    property XorStr: AnsiString read FXorStr write FXorStr;
    {* ���ڼ��ܴ�����ַ��� }
  end;

//==============================================================================
// �ַ�ӳ���ʽ���ܵ� TStream ��
//==============================================================================

{ TCnCodeMapStream }

  TCnCodeMap = array[Byte] of Byte;

  TCnCodeMapStream = class (TCnEncryptStream)
  {* �ַ�ӳ���ʽ���ܵ� TStream �֧࣬�����ݶ�дʱ���м��ܴ���}
  private
    FSeedStr: AnsiString;
    FEnMap: TCnCodeMap;
    FDeMap: TCnCodeMap;
    procedure SetSeedStr(const Value: AnsiString);
  protected
    procedure DeEncrypt(var Buffer; Count: Longint); override;
    procedure Encrypt(var Buffer; Count: Longint); override;
  public
    constructor Create(AStream: TStream; const ASeedStr: AnsiString;
      AOwned: Boolean = False);
    {* �๹����
     |<PRE>
       AStream: TStream         - ��Ҫ���м��ܴ������
       ASeedStr: string         - ���ڼ��ܴ���������ַ���
       AOwned: Boolean          - �Ƿ����ͷż�����ʱͬʱ�ͷ� AStream
     |</PRE>}
    property SeedStr: AnsiString read FSeedStr write SetSeedStr;
    {* ���ڼ��ܴ�����ַ��� }
  end;

function CnFastMemoryStreamCopyFrom(Dest, Source: TStream; Count: Int64): Int64;
{* ���ٵ� MemoryStream �� CopyFrom ���������� Dest �� Source ֮һ�� MemoryStream
   �������ֱ�Ӷ�д�ڴ棬�����˷��仺�������ظ���д�Ŀ������紫����� Stream ��
   ���� TCustomMemoryStream�������ԭ CopyFrom ������}

function CnGenerateCodeMap(ASeedStr: AnsiString; var EnMap, DeMap: TCnCodeMap): Boolean;
{* ����һ���ַ������������ֽڼӽ��ܵı�����ַ���Ϊ��ʱ����Ϊ False *}

implementation

resourcestring
  SCnReadStreamError = 'Read stream error';
  SCnWriteStreamError = 'Write stream error';

const
  csBeginFlagInt = Longint($00FF00FF);
  csEndFlagInt = Longint($FF00FF00);
  csDefSeedStr = '{A53108FC-BD75-42B8-BD10-DA3DC166D0B0}';

function CnFastMemoryStreamCopyFrom(Dest, Source: TStream; Count: Int64): Int64;
var
  aNewSize: Longint;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  //ע��˷����ܹ�������Ҫ������ Dest��Source ����֮һ�� TCustomMemoryStream���������ԭ����
  
  //�ж� Dest �� Source �ǲ��� TCustomMemoryStream������ǵĻ���ֱ�Ӵ� Memory �ĵ�ַ�Ͻ��в��� ,
  //��Ϊ CustomMemoryStream �� Read �� Write ���õ� System.Move
  //����ʡȥ��ԭ TStream ��ͣ�ش�����д Buffer ������ Capacity��ԭ Copyfrom �������˷�ʱ��
  //�ô˷�����ֱ�ӽ� Source ���ڴ��׷�ӵ� Dest �� Memory ��ַ�ĺ���

  if Source is TCustomMemoryStream then     // ֱ��д�뵽 Dest �ĵ�ַ��
    Dest.WriteBuffer(Pointer(Longint(TCustomMemoryStream(Source).Memory) + Source.Position)^,Count)
  else if Dest is TCustomMemoryStream then
  begin
    aNewSize := Dest.Position + Count;
    TCustomMemoryStream(Dest).Size := aNewSize; //�������ڴ�Ĵ�С����Dest.Memory����,��Ȼ�����Ҳ�����ַ
    // Source ֱ�Ӵ� Dest.Memory ����д
    Source.ReadBuffer(Pointer(Longint(TCustomMemoryStream(Dest).Memory) + Dest.Position)^, Count);
  end
  else
  begin
    Dest.CopyFrom(Source, Count);
  end;
end;

function CnGenerateCodeMap(ASeedStr: AnsiString; var EnMap, DeMap: TCnCodeMap): Boolean;
var
  I: Integer;
  C: Byte;
  List: TList;
begin
  Result := False;
  if ASeedStr = '' then
    Exit;

  List := TList.Create;
  try
    for I := 0 to 255 do
      List.Add(Pointer(I));
    for I := 0 to 255 do
    begin
      C := Byte(ASeedStr[I mod Length(ASeedStr) + 1]) xor $3E;
      C := (C * 3 + 7) mod List.Count;
      EnMap[I] := Byte(List[C]);
      DeMap[Byte(List[C])] := I;
      List.Delete(C);
    end;
    Result := True;
  finally
    List.Free;
  end;
end;

//==============================================================================
// ��չ�� TStream ��
//==============================================================================

{ TCnStream }

constructor TCnStream.Create(AStream: TStream; AOwned: Boolean);
begin
  inherited Create;
  Assert(Assigned(AStream));
  FStream := AStream;
  FOwned := AOwned;
end;

constructor TCnStream.Create;
begin
  Create(TMemoryStream.Create, True);
end;

constructor TCnStream.Create(const FileName: string; Mode: Word);
begin
  Create(TFileStream.Create(FileName, Mode), True);
end;

destructor TCnStream.Destroy;
begin
  if FOwned then
    FreeAndNil(FStream);
  inherited;
end;

class procedure TCnStream.ReadError;
begin
  raise ECnReadStreamError.CreateRes(@SCnReadStreamError);
end;

class procedure TCnStream.WriteError;
begin
  raise ECnWriteStreamError.CreateRes(@SCnWriteStreamError);
end;

//------------------------------------------------------------------------------
// ���ñ���װ�� Stream ���ʷ���
//------------------------------------------------------------------------------

{$IFDEF COMPILER7_UP}
function TCnStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;
{$ENDIF}

function TCnStream.Read(var Buffer; Count: LongInt): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TCnStream.Seek(Offset: LongInt; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TCnStream.Write(const Buffer; Count: LongInt): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

procedure TCnStream.SetSize(NewSize: LongInt);
begin
  FStream.Size := NewSize;
end;

{$IFDEF COMPILER6_UP}
procedure TCnStream.SetSize(const NewSize: Int64);
begin
  FStream.Size := NewSize;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// ���ݿ��־��������
//------------------------------------------------------------------------------

procedure TCnStream.BeginRead(Flag: LongInt);
begin
  if Flag = -1 then Flag := csBeginFlagInt;
  if ReadInteger <> Flag then ReadError;
end;

procedure TCnStream.EndRead(Flag: LongInt);
begin
  if Flag = -1 then Flag := csEndFlagInt;
  if ReadInteger <> Flag then ReadError;
end;

procedure TCnStream.BeginWrite(Flag: LongInt);
begin
  if Flag = -1 then Flag := csBeginFlagInt;
  WriteInteger(Flag);
end;

procedure TCnStream.EndWrite(Flag: LongInt);
begin
  if Flag = -1 then Flag := csEndFlagInt;
  WriteInteger(Flag);
end;

//------------------------------------------------------------------------------
// ��������
//------------------------------------------------------------------------------

procedure TCnStream.DoRead(var Buffer; Count: LongInt);
begin
  if Read(Buffer, Count) <> Count then
    ReadError;
end;

procedure TCnStream.DoWrite(const Buffer; Count: LongInt);
begin
  if Write(Buffer, Count) <> Count then WriteError;
end;

function TCnStream.ReadDataType: TCnStreamDataType;
begin
  DoRead(Result, SizeOf(Result));
end;

procedure TCnStream.WriteDataType(DataType: TCnStreamDataType);
begin
  DoWrite(DataType, SizeOf(DataType));
end;

//------------------------------------------------------------------------------
// ��չ�����ݴ�ȡ����
//------------------------------------------------------------------------------

function TCnStream.ReadBool: Boolean;
begin
  if ReadDataType <> dtBool then ReadError;
  DoRead(Result, SizeOf(Result));
end;

procedure TCnStream.ReadData(var Buffer; Count: LongInt);
var
  ACount: Integer;
begin
  if ReadDataType <> dtData then ReadError;
  DoRead(ACount, SizeOf(ACount));
  if ACount <> Count then ReadError;
  DoRead(Buffer, Count);
end;

function TCnStream.ReadDateTime: TDateTime;
begin
  if ReadDataType <> dtDateTime then ReadError;
  DoRead(Result, SizeOf(Result));
end;

function TCnStream.ReadFloat: Double;
begin
  if ReadDataType <> dtDouble then ReadError;
  DoRead(Result, SizeOf(Result));
end;

function TCnStream.ReadInteger: Longint;
begin
  if ReadDataType <> dtInteger then ReadError;
  DoRead(Result, SizeOf(Result));
end;

function TCnStream.ReadString: string;
var
  Len: Integer;
begin
  if ReadDataType <> dtString then ReadError;
  DoRead(Len, SizeOf(Len));
  if Len > 0 then
  begin
    SetLength(Result, Len);
    DoRead(PChar(Result)^, Len);
  end
  else
    Result := '';
end;

procedure TCnStream.WriteBool(Value: Boolean);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtBool;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Value, SizeOf(Value));
end;

procedure TCnStream.WriteData(const Buffer; Count: LongInt);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtData;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Count, SizeOf(Count));
  DoWrite(Buffer, Count);
end;

procedure TCnStream.WriteDateTime(Value: TDateTime);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtDateTime;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Value, SizeOf(Value));
end;

procedure TCnStream.WriteFloat(Value: Double);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtDouble;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Value, SizeOf(Value));
end;

procedure TCnStream.WriteInteger(Value: LongInt);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtInteger;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Value, SizeOf(Value));
end;

procedure TCnStream.WriteString(Value: string);
var
  DataType: TCnStreamDataType;
  Len: Integer;
begin
  DataType := dtString;
  DoWrite(DataType, SizeOf(DataType));
  Len := Length(Value);
  DoWrite(Len, SizeOf(Len));
  if Len > 0 then
    DoWrite(PChar(Value)^, Len);
end;

//------------------------------------------------------------------------------
// ���Զ�д����
//------------------------------------------------------------------------------

function TCnStream.GetHandle: Integer;
begin
  if FStream is THandleStream then
    Result := THandleStream(FStream).Handle
  else
    Result := -1;
end;

function TCnStream.GetMemory: Pointer;
begin
  if FStream is TCustomMemoryStream then
    Result := TCustomMemoryStream(FStream).Memory
  else
    Result := nil;
end;

//==============================================================================
// ���ܵ� TStream �������
//==============================================================================

{ TCnEncryptStream }

constructor TCnEncryptStream.Create(AStream: TStream; AOwned: Boolean);
begin
  inherited Create;
  Assert(Assigned(AStream));
  FStream := AStream;
  FOwned := AOwned;
end;

destructor TCnEncryptStream.Destroy;
begin
  if FOwned then
    FreeAndNil(FStream);
  inherited;
end;

procedure TCnEncryptStream.DoAfterDeEncrypt(const Buffer; Count: LongInt);
begin

end;

procedure TCnEncryptStream.DoAfterEncrypt(const Buffer; Count: LongInt);
begin

end;

procedure TCnEncryptStream.DoBeforeDeEncrypt(const Buffer; Count: LongInt);
begin

end;

procedure TCnEncryptStream.DoBeforeEncrypt(const Buffer; Count: LongInt);
begin

end;

//------------------------------------------------------------------------------
// ���ñ���װ�� Stream ���ʷ���
//------------------------------------------------------------------------------

{$IFDEF COMPILER7_UP}
function TCnEncryptStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;
{$ENDIF}

function TCnEncryptStream.Read(var Buffer; Count: Longint): LongInt;
begin
  Result := FStream.Read(Buffer, Count);
  DoBeforeDeEncrypt(Buffer, Count);
  DeEncrypt(Buffer, Count);
  DoAfterDeEncrypt(Buffer, Count);
end;

function TCnEncryptStream.Seek(Offset: Longint; Origin: Word): LongInt;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TCnEncryptStream.SetSize(NewSize: LongInt);
begin
  FStream.Size := NewSize;
end;

{$IFDEF COMPILER6_UP}
procedure TCnEncryptStream.SetSize(const NewSize: Int64);
begin
  FStream.Size := NewSize;
end;
{$ENDIF}

function TCnEncryptStream.Write(const Buffer; Count: Longint): LongInt;
var
  MemBuff: Pointer;
begin
  GetMem(MemBuff, Count);
  try
    DoBeforeEncrypt(Buffer, Count);
    Move(Buffer, MemBuff^, Count);
    // CopyMemory(MemBuff, @Buffer, Count);
    Encrypt(MemBuff^, Count);
    DoAfterEncrypt(Buffer, Count);
    Result := FStream.Write(MemBuff^, Count);
  finally
    FreeMem(MemBuff);
  end;
end;

//==============================================================================
// Xor ��ʽ���ܵ� TStream ��
//==============================================================================

{ TCnXorStream }

constructor TCnXorStream.Create(AStream: TStream; const AXorStr: AnsiString;
  AOwned: Boolean);
begin
  inherited Create(AStream, AOwned);
  FXorStr := AXorStr;
end;

procedure TCnXorStream.Encrypt(var Buffer; Count: Longint);
var
  I, p, l: Integer;
begin
  l := Length(FXorStr);
  if l > 0 then
  begin
    p := FSeedPos;
    for I := 0 to Count - 1 do
      PByteArray(@Buffer)^[I] := PByteArray(@Buffer)^[I] xor
        Byte(FXorStr[(p + I) mod l + 1]);
  end;
end;

procedure TCnXorStream.DeEncrypt(var Buffer; Count: Longint);
begin
  Encrypt(Buffer, Count);
end;

procedure TCnXorStream.DoAfterEncrypt(const Buffer; Count: LongInt);
begin

end;

procedure TCnXorStream.DoBeforeEncrypt(const Buffer; Count: LongInt);
begin
  // ��дǰ����Ҫ��¼λ�ã������е� xor ���ܵ������ַ�λ�öԵ��Ϻ�
  FSeedPos := Position;
end;

procedure TCnXorStream.DoAfterDeEncrypt(const Buffer; Count: LongInt);
begin

end;

procedure TCnXorStream.DoBeforeDeEncrypt(const Buffer; Count: LongInt);
begin
  // ��дǰ����Ҫ��¼λ�ã������е� xor ���ܵ������ַ�λ�öԵ��Ϻ�
  FSeedPos := Position - Count;
end;

//==============================================================================
// �ַ�ӳ���ʽ���ܵ� TStream ��
//==============================================================================

{ TCnCodeMapStream }

constructor TCnCodeMapStream.Create(AStream: TStream;
  const ASeedStr: AnsiString; AOwned: Boolean);
begin
  inherited Create(AStream, AOwned);
  SeedStr := ASeedStr;
end;

procedure TCnCodeMapStream.DeEncrypt(var Buffer; Count: LongInt);
var
  I: Integer;
  P: PByte;
begin
  P := PByte(@Buffer);
  for I := 0 to Count - 1 do
  begin
    P^ := FDeMap[P^];
    Inc(P);
  end;
end;

procedure TCnCodeMapStream.Encrypt(var Buffer; Count: LongInt);
var
  I: Integer;
  P: PByte;
begin
  P := PByte(@Buffer);
  for I := 0 to Count - 1 do
  begin
    P^ := FEnMap[P^];
    Inc(P);
  end;
end;

procedure TCnCodeMapStream.SetSeedStr(const Value: AnsiString);
begin
  FSeedStr := Value;
  if FSeedStr = '' then
    CnGenerateCodeMap(csDefSeedStr, FEnMap, FDeMap)
  else
    CnGenerateCodeMap(FSeedStr, FEnMap, FDeMap);
end;

end.
