{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnWideStrings;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：WideStrings 单元
* 单元作者：CnPack 开发组
* 备    注：该单元实现了简化的 TCnWideStringList 类
*           以及扩展的 UTF8 到 UTF16 的编解码函数，支持 UTF16 中的四字节字符与 UTF8-MB4
* 开发平台：WinXP SP3 + Delphi 5.0
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：202.11.10 V1.1
*               UTF8 编码解码支持 UTF8-MB4 与 UTF16 中的四字节字符
*           2010.01.16 by ZhouJingyu
*               初始化提交
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, IniFiles;

type

{ TCnWideStringList }

  TCnWideListFormat = (wlfAnsi, wlfUtf8, wlfUnicode);

  TCnWideStringList = class;
  TCnWideStringListSortCompare = function(List: TCnWideStringList; Index1, Index2: Integer): Integer;

  PCnWideStringItem = ^TCnWideStringItem;
  TCnWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  TCnWideStringList = class(TPersistent)
  private
    FList: TList;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure SetValue(const Name, Value: WideString);
    procedure QuickSort(L, R: Integer; SCompare: TCnWideStringListSortCompare);
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
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;
  end;

{ TCnWideMemIniFile }

  TCnWideMemIniFile = class(TMemIniFile)
  public
    constructor Create(const AFileName: string);
    procedure UpdateFile; override;
  end;

function CnUtf8EncodeWideString(const S: WideString): AnsiString;
{* 对 WideString 进行 Utf8 编码得到 AnsiString，不做 Ansi 转换避免丢字符
  支持四字节 UTF16 字符与 UTF8-MB4}

function CnUtf8DecodeToWideString(const S: AnsiString): WideString;
{* 对 AnsiString 的 Utf8 解码得到 WideString，不做 Ansi 转换避免丢字符
  支持四字节 UTF16 字符与 UTF8-MB4}

implementation

uses
  CnGB18030;

{ TCnWideStringList }

function WideCompareText(const S1, S2: WideString): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(S1),
    Length(S1), PWideChar(S2), Length(S2)) - 2;
end;

function TCnWideStringList.Add(const S: WideString): Integer;
begin
  Result := Count;
  Insert(Count, S);
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
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TCnWideStringList.GetTextStr: WideString;
var
  I, L, Size, C: Integer;
  P: PwideChar;
  S, LB: WideString;
begin
  C := GetCount;
  Size := 0;
  LB := #13#10;
  for I := 0 to C - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to C - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L*SizeOf(WideChar));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L*SizeOf(WideChar));
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
    if WideCompareText(Get(Result), S) = 0 then Exit;
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
    if (P <> 0) and (WideCompareText(Copy(S, 1, P - 1), Name) = 0) then Exit;
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
    if HeaderStr = #$EF#$BB#$BF then // utf-8 format
    begin
      SetLength(SA, Size - 3);
      Stream.Read(Pointer(SA)^, Size - 3);
      Len := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(SA), -1, nil, 0);
      SetLength(S, Len);
      MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(SA), -1, PWideChar(S), Len);
      SetTextStr(S);
      Exit;
    end;
    Stream.Position := Stream.Position - 3;  
  end;

  if Size >= 2 then
  begin
    SetLength(HeaderStr, 2);
    Stream.Read(Pointer(HeaderStr)^, 2);
    if HeaderStr = #$FF#$FE then // utf-8 format
    begin
      SetLength(S, (Size - 2) div SizeOf(WideChar));
      Stream.Read(Pointer(S)^, (Size - 2) div SizeOf(WideChar) * SizeOf(WideChar));
      SetTextStr(S);
      Exit;
    end;
    Stream.Position := Stream.Position - 2;  
  end;
      
  SetString(SA, nil, Size);
  Stream.Read(Pointer(SA)^, Size);
  SetTextStr({$IFDEF UNICODE}string{$ENDIF}(SA));
end;

procedure TCnWideStringList.Put(Index: Integer; const S: WideString);
var
  P: PCnWideStringItem;
begin
  P := PCnWideStringItem(FList[Index]);
  P^.FString := S;
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
    Len := WideCharToMultiByte(CP_UTF8, 0, PWideChar(S), -1, nil, 0, nil, nil);
    SetLength(SA, Len);
    WideCharToMultiByte(CP_UTF8, 0, PWideChar(S), -1, PAnsiChar(SA), Len, nil, nil);
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

{ TCnWideMemIniFile }

constructor TCnWideMemIniFile.Create(const AFileName: string);
var
  WList: TCnWideStringList;
  List: TStringList;
begin
  inherited Create(AFileName);
  WList := nil;
  List := nil;
  try
    WList := TCnWideStringList.Create;
    WList.LoadFromFile(AFileName);
    List := TStringList.Create;
    List.Text := WList.Text;
    SetStrings(List);
  finally
    WList.Free;
    List.Free;
  end;   
end;

procedure TCnWideMemIniFile.UpdateFile;
var
  WList: TCnWideStringList;
  List: TStringList;
begin
  WList := nil;
  List := nil;
  try
    List := TStringList.Create;
    GetStrings(List);
    WList := TCnWideStringList.Create;
    WList.Text := List.Text;
    WList.SaveToFile(FileName, wlfUtf8);
  finally
    WList.Free;
    List.Free;
  end;   
end;

// D5 下没有内置 UTF8/Ansi 转换函数，且低版本即使有也不支持 UTF8-MB4，因此写个替代品
// 为调用者简明起见，SourceChars 传宽字符个数即可
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
        // 本字符是四字节，要特殊编码
        C := GetCodePointFromUtf164Char(PAnsiChar(@(Source[I])));
        Inc(I, 2); // 步进两个 WideChar
      end
      else
      begin
        C := Cardinal(Source[I]);
        Inc(I); // 步进一个 WideChar
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
        // 本字符是四字节，要特殊编码
        C := GetCodePointFromUtf164Char(PAnsiChar(@(Source[I])));
        Inc(I, 2); // 步进两个 WideChar
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
        if I >= SourceBytes then Exit;          // incomplete multibyte char

        if (WC and $F0) = $F0 then              // 四字节，单独处理，再步进三个字符，拼成字符值，再算成四字节的 UTF16 编码
        begin
          if SourceBytes - I < 3 then Exit;     // 不够四字节则出错退出

          // WC 是第一个字节，取低三位，后面仨字节各取低六位，得到码点
          WC := ((WC and $7) shl 18) + ((Cardinal(Source[I]) and $3F) shl 12)
            + ((Cardinal(Source[I + 1]) and $3F) shl 6) + (Cardinal(Source[I + 2]) and $3F);

          // 根据码点生成 UTF16 字符，并步进 Cnt
          K := GetUtf16CharFromCodePoint(WC, @(Dest[Cnt]));
          if K = 2 then // 生成了四字节字符，先步进一个 WideChar，下一个放 if 后步进
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
            if (C and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
            if I >= SourceBytes then Exit;        // incomplete multibyte char
            WC := (WC shl 6) or (C and $3F);
          end;
          C := Byte(Source[I]);
          Inc(I);
          if (C and $C0) <> $80 then Exit;       // malformed trail byte

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

      if (C and $80) <> 0 then                  // 最高位为 1，至少二字节
      begin
        if I >= SourceBytes then                // incomplete multibyte char
          Exit;

        C := C and $3F;                         // 留下第一个字节的低六位，前两位已经当成 11 了
        if (C and $20) <> 0 then                // 如果是 1110，则表示至少有仨字节
        begin
          if (C and $10) <> 0 then              // 如果是 11110，则表示共有四字节
          begin
            C := Byte(Source[I]);               // 读第四个中的第二个字节
            Inc(I);
            if (C and $C0) <> $80 then          // 该字节最高两位得是 10
              Exit;                             // malformed trail byte or out of range char
            if I >= SourceBytes then
              Exit;                             // incomplete multibyte char

            Inc(Cnt);                           // 四字节的 UTF8，应对应 UTF16 中的两个 WideChar，这里额外加一
          end;

          C := Byte(Source[I]);                 // 读四个中的第三个字节，或三个中的第二个字节
          Inc(I);
          if (C and $C0) <> $80 then            // 该字节最高两位得是 10，否则退出
            Exit;
          if I >= SourceBytes then
            Exit;                               // incomplete multibyte char
        end;

        C := Byte(Source[I]);                   // 读四个中的第四个字节，或三个中的第三个字节，或二个中的第二个字节
        Inc(I);
        if (C and $C0) <> $80 then              // 该字节最高两位得是 10，否则退出
          Exit;                                 // malformed trail byte
      end;

      Inc(Cnt);
    end;
  end;
  Result := Cnt + 1;
end;

// 对 WideString 进行 Utf8 编码得到 AnsiString，不做 Ansi 转换避免丢字符
function CnUtf8EncodeWideString(const S: WideString): AnsiString;
var
  L: Integer;
  Temp: AnsiString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S) * 3); // SetLength includes space for null terminator

  L := InternalUnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

// 对 AnsiString 的 Utf8 解码得到 WideString，不做 Ansi 转换避免丢字符
function CnUtf8DecodeToWideString(const S: AnsiString): WideString;
var
  L: Integer;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Result, Length(S));

  L := InternalUtf8ToUnicode(PWideChar(Result), Length(Result) + 1, PAnsiChar(S), Length(S));
  if L > 0 then
    SetLength(Result, L - 1)
  else
    Result := '';
end;

end.
