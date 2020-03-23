{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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

unit CnPemUtils;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：处理 PEM 格式加载以及加解密单元
* 单元作者：刘啸
* 备    注：
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.03.23 V1.1
*               模拟 OPENSSL 实现 PEM 的加密读取，只支持部分加密算法与机制
*           2020.03.18 V1.0
*               创建单元，从 CnRSA 中独立出来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnBase64, CnAES, CnMD5, CnSHA2;

type
  TCnKeyHashMethod = (ckmMd5, ckmSha256);

function LoadPemFileToMemory(const FileName, ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream; const Password: string = '';
  KeyHashMethod: TCnKeyHashMethod = ckmMd5): Boolean;
{* 从 PEM 格式编码的文件中验证指定头尾后读入实际内容并进行 Base64 解码}

function LoadPemStreamToMemory(Stream: TStream; const ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream; const Password: string = '';
  KeyHashMethod: TCnKeyHashMethod = ckmMd5): Boolean;
{* 从 PEM 格式编码的文件中验证指定头尾后读入实际内容并进行 Base64 解码}

function SaveMemoryToPemFile(const FileName, Head, Tail: string;
  MemoryStream: TMemoryStream): Boolean;
{* 将 Stream 的内容进行 Base64 编码后分行并补上文件头尾再写入文件}

implementation

const
  ENC_HEAD_PROCTYPE = 'Proc-Type:';
  ENC_HEAD_PROCTYPE_NUM = '4';
  ENC_HEAD_ENCRYPTED = 'ENCRYPTED';
  ENC_HEAD_DEK = 'DEK-Info:';

  ENC_TYPE_AES128 = 'AES-128';
  ENC_TYPE_AES192 = 'AES-192';
  ENC_TYPE_AES256 = 'AES-256';

  ENC_BLOCK_CBC   = 'CBC';

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function StrToHex(Value: PAnsiChar; Len: Integer): AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Len - 1 do
    Result := Result + IntToHex(Ord(Value[I]), 2);
end;

function HexToInt(Hex: AnsiString): Integer;
var
  I, Res: Integer;
  ch: AnsiChar;
begin
  Res := 0;
  for I := 0 to Length(Hex) - 1 do
  begin
    ch := Hex[I + 1];
    if (ch >= '0') and (ch <= '9') then
      Res := Res * 16 + Ord(ch) - Ord('0')
    else if (ch >= 'A') and (ch <= 'F') then
      Res := Res * 16 + Ord(ch) - Ord('A') + 10
    else if (ch >= 'a') and (ch <= 'f') then
      Res := Res * 16 + Ord(ch) - Ord('a') + 10
    else raise Exception.Create('Error: not a Hex String');
  end;
  Result := Res;
end;

function HexToStr(Value: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    if ((I mod 2) = 1) then
      Result := Result + AnsiChar(HexToInt(Copy(Value, I, 2)));
  end;
end;

// 拿加密算法、块运算、初始化向量，密码来解开 Base64 编码的 S，再写入 Stream 内
function DecryptPemString(const S, M1, M2, HexIv, Password: string; Stream: TMemoryStream;
  KeyHashMethod: TCnKeyHashMethod): Boolean;
var
  DS: TMemoryStream;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
  IvStr: AnsiString;
  AesIv: TAESBuffer;
  Md5Dig: TMD5Digest;
  Sha256Dig: TSHA256Digest;
begin
  Result := False;
  DS := nil;

  if (M1 = '') or (M2 = '') or (HexIv = '') or (Password = '') then
    Exit;

  try
    DS := TMemoryStream.Create;
    if BASE64_OK <> Base64Decode(S, DS, False) then
      Exit;

    DS.Position := 0;
    IvStr := HexToStr(HexIv);

    // DS 中是密文，要解到 Stream 中
    if (M1 = ENC_TYPE_AES256) and (M2 = ENC_BLOCK_CBC) and (KeyHashMethod = ckmMd5) then
    begin
      // 解开 AES-256-CBC 加密的密文，此处测试未通过
      FillChar(AESKey256, SizeOf(AESKey256), 0);
      Md5Dig := MD5String(Password + Copy(IvStr, 1, 8));
      Move(Md5Dig, AESKey256, Min(SizeOf(AESKey256), SizeOf(TMD5Digest)));
      // Move(Md5Dig[0], Key[1], SizeOf(Md5Dig));
      // Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));

      Move(IvStr[1], AesIv, Min(SizeOf(TAESBuffer), Length(IvStr)));
      DecryptAESStreamCBC(DS, DS.Size, AESKey256, AesIv, Stream);
      Result := True;
    end
    else if (M1 = ENC_TYPE_AES128) and (M2 = ENC_BLOCK_CBC) and (KeyHashMethod = ckmMd5) then
    begin
      // 解开 AES-128-CBC 加密的密文，此处测试通过。
      // 密码与 Iv 的前八字节拼起来的 MD5 作为 Key
      FillChar(AESKey128, SizeOf(AESKey128), 0);
      Md5Dig := MD5String(Password + Copy(IvStr, 1, 8));
      Move(Md5Dig, AESKey128, Min(SizeOf(AESKey128), SizeOf(TMD5Digest)));

      Move(IvStr[1], AesIv, Min(SizeOf(TAESBuffer), Length(IvStr)));
      DecryptAESStreamCBC(DS, DS.Size, AESKey128, AesIv, Stream);
      Result := True;
    end
  finally
    DS.Free;
  end;
end;

function LoadPemStreamToMemory(Stream: TStream; const ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream; const Password: string; KeyHashMethod: TCnKeyHashMethod): Boolean;
var
  I, J: Integer;
  S, L1, L2, M1, M2, M3: string;
  Sl: TStringList;
begin
  Result := False;

  if (Stream <> nil) and (Stream.Size > 0) and (ExpectHead <> '') and (ExpectTail <> '') then
  begin
    Sl := TStringList.Create;
    try
      Sl.LoadFromStream(Stream);
      if Sl.Count > 2 then
      begin
        if Trim(Sl[0]) <> ExpectHead then
          Exit;

        if Trim(Sl[Sl.Count - 1]) = '' then // 去掉末尾可能的空行
          Sl.Delete(Sl.Count - 1);

        if Trim(Sl[Sl.Count - 1]) <> ExpectTail then
          Exit;

        // 头尾验证通过，读前两行判断是否加密
        L1 := Sl[1];
        if Pos(ENC_HEAD_PROCTYPE, L1) = 1 then // 是加密的
        begin
          Delete(L1, 1, Length(ENC_HEAD_PROCTYPE));
          I := Pos(',', L1);
          if I <= 1 then
            Exit;

          if Trim(Copy(L1, 1, I - 1)) <> ENC_HEAD_PROCTYPE_NUM then
            Exit;

          if Trim(Copy(L1, I + 1, MaxInt)) <> ENC_HEAD_ENCRYPTED then
            Exit;

          // ProcType: 4,ENCRYPTED 判断通过

          L2 := Sl[2];
          if Pos(ENC_HEAD_DEK, L2) <> 1 then
            Exit;

          Delete(L2, 1, Length(ENC_HEAD_DEK));
          I := Pos(',', L2);
          if I <= 1 then
            Exit;

          M1 := Trim(Copy(L2, 1, I - 1)); // 得到 AES256-CBC 这种
          M3 := UpperCase(Trim(Copy(L2, I + 1, MaxInt)));  // 得到加密时使用的初始化向量
          I := Pos('-', M1);
          if I <= 1 then
            Exit;
          J := Pos('-', Copy(M1, I + 1, MaxInt));
          if J > 0 then
            I := I + J; // AES-256-CBC

          M2 := UpperCase(Trim(Copy(M1, I + 1, MaxInt)));  // 得到块模式，如 ECB 或 CBC 等
          M1 := UpperCase(Trim(Copy(M1, 1, I - 1)));       // 得到加密算法，如 DES 或 AES 等

          // 头尾和这两行全删掉
          Sl.Delete(Sl.Count - 1);
          Sl.Delete(0);
          Sl.Delete(0);
          Sl.Delete(0);

          S := '';
          for I := 0 to Sl.Count - 1 do
            S := S + Sl[I];

          S := Trim(S);

          Result := DecryptPemString(S, M1, M2, M3, Password, MemoryStream, KeyHashMethod);
        end
        else // 未加密的，拼凑成 Base64 后解密
        begin
          Sl.Delete(Sl.Count - 1);
          Sl.Delete(0);
          S := '';
          for I := 0 to Sl.Count - 1 do
            S := S + Sl[I];

          S := Trim(S);

          // To De Base64 S
          MemoryStream.Clear;
          Result := (BASE64_OK = Base64Decode(S, MemoryStream, False));
        end;
      end;
    finally
      Sl.Free;
    end;
  end;
end;

function LoadPemFileToMemory(const FileName, ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream; const Password: string; KeyHashMethod: TCnKeyHashMethod): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadPemStreamToMemory(Stream, ExpectHead, ExpectTail, MemoryStream, Password, KeyHashMethod);
  finally
    Stream.Free;
  end;
end;

procedure SplitStringToList(const S: string; List: TStrings);
const
  LINE_WIDTH = 64;
var
  C, R: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  if S <> '' then
  begin
    R := S;
    while R <> '' do
    begin
      C := Copy(R, 1, LINE_WIDTH);
      Delete(R, 1, LINE_WIDTH);
      List.Add(C);
    end;
  end;
end;

function SaveMemoryToPemFile(const FileName, Head, Tail: string;
  MemoryStream: TMemoryStream): Boolean;
var
  S: string;
  List: TStringList;
begin
  Result := False;
  if (MemoryStream <> nil) and (MemoryStream.Size <> 0) then
  begin
    MemoryStream.Position := 0;
    if Base64_OK = Base64Encode(MemoryStream, S) then
    begin
      List := TStringList.Create;
      try
        SplitStringToList(S, List);

        List.Insert(0, Head);
        List.Add(Tail);

        List.SaveToFile(FileName);
        Result := True;
      finally
        List.Free;
      end;
    end;
  end;
end;

end.
