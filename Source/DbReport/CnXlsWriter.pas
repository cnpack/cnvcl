{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

unit CnXlsWriter;
{* |<PRE>
================================================================================
* 软件名称：开发包数据库类
* 单元名称：简单的XLS文件生成类
* 单元作者：solokey
* 备    注：简单的XLS文件生成类，据格式手工读写生成。
*           Col的范围 0-255，Row的范围0-MaxWord，超出范围的话,文件打不开。
*           生成的xls文件只有一个Sheet, Sheet名与xls的文件名相同。
* 开发平台：PWinXP + Delphi 7
* 兼容测试：PWin2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.04.30 V1.0
*               solokey从原始代码移植而来。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Sysutils, ComObj{$IFDEF COMPILER6_UP}, Variants{$ENDIF};
 
type
  TCnXlsWriter = class(TObject)
  private
    FStream: TMemoryStream;
    FADOCompatible: Boolean;
    procedure SetCells(const ACol: Byte; const ARow: Word; const Value: Variant);
  protected
    procedure XlsBeginStream(XlsStream: TStream; const BuildNumber: Word);
    procedure XlsEndStream(XlsStream: TStream);
    procedure XlsWriteCellRk(XlsStream: TStream; const ACol: Byte; const ARow: Word;
      const AValue: Integer);
    procedure XlsWriteCellNumber(XlsStream: TStream; const ACol: Byte; const ARow: Word;
      const AValue: Double);
    procedure XlsWriteCellLabel(XlsStream: TStream; const ACol: Byte; const ARow: Word;
      const AValue: string);
    procedure XlsWriteCellBlank(XlsStream: TStream; const ACol: Byte; const ARow: Word);
  public
    constructor Create;
    destructor  Destroy; override;
    
    procedure   NewXls;
    {* 将文件清空}
    
    procedure   SaveToXls(const FileName: string);
    {* 保存文件}
    
    property    ADOCompatible: Boolean read FADOCompatible write FADOCompatible;
    {* 兼容ADO，如果需要兼容 ADO，则内部将文件使用Excel Application 来重新保存}
    
    property    Cells[const ACol: Byte; const ARow: Word]: Variant write SetCells;
    {* 写单元格内容的属性，不可读}
  end;

implementation

const
  CXlsBof: array[0..5] of Word = ($809, 8, 00, $10, 0, 0);
  CXlsEof: array[0..1] of Word = ($0A, 00);
  CXlsLabel: array[0..5] of Word = ($204, 0, 0, 0, 0, 0);
  CXlsNumber: array[0..4] of Word = ($203, 14, 0, 0, 0);
  CXlsRk: array[0..4] of Word = ($27E, 10, 0, 0, 0);
  CXlsBlank: array[0..4] of Word = ($201, 6, 0, 0, $17);

procedure TCnXlsWriter.XlsBeginStream(XlsStream: TStream; const BuildNumber: Word);
begin
  CXlsBof[4] := BuildNumber;
  XlsStream.WriteBuffer(CXlsBof, SizeOf(CXlsBof));
end;

procedure TCnXlsWriter.XlsEndStream(XlsStream: TStream);
begin
  XlsStream.WriteBuffer(CXlsEof, SizeOf(CXlsEof));
end;

procedure TCnXlsWriter.XlsWriteCellRk(XlsStream: TStream; const ACol: Byte; const ARow: Word;
  const AValue: Integer);
var
  V: Integer;
begin
  CXlsRk[2] := ARow;
  CXlsRk[3] := ACol;
  XlsStream.WriteBuffer(CXlsRk, SizeOf(CXlsRk));
  V := (AValue shl 2) or 2;
  XlsStream.WriteBuffer(V, 4);
end;

procedure TCnXlsWriter.XlsWriteCellNumber(XlsStream: TStream; const ACol: Byte; const ARow: Word;
  const AValue: Double);
begin
  CXlsNumber[2] := ARow;
  CXlsNumber[3] := ACol;
  XlsStream.WriteBuffer(CXlsNumber, SizeOf(CXlsNumber));
  XlsStream.WriteBuffer(AValue, 8);
end;

procedure TCnXlsWriter.XlsWriteCellLabel(XlsStream: TStream; const ACol: Byte; const ARow: Word;
  const AValue: string);
var
  L: Word;
begin
  L := Length(AValue);
  CXlsLabel[1] := 8 + L;
  CXlsLabel[2] := ARow;
  CXlsLabel[3] := ACol;
  CXlsLabel[5] := L;
  XlsStream.WriteBuffer(CXlsLabel, SizeOf(CXlsLabel));
  XlsStream.WriteBuffer(Pointer(AValue)^, L);
end;


procedure TCnXlsWriter.XlsWriteCellBlank(XlsStream: TStream; const ACol: Byte;
const ARow: Word);
begin
  CXlsBlank[2] := ARow;
  CXlsBlank[3] := ACol;
  XlsStream.WriteBuffer(CXlsBlank, SizeOf(CXlsBlank));
end;

constructor TCnXlsWriter.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FADOCompatible := False;
  NewXls;
end;

destructor TCnXlsWriter.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TCnXlsWriter.SaveToXls(const FileName: string);
var
  ExcelApp: Variant;
  ExcelWork: Variant;
  TargetFileName: string;
begin
  if Trim(FileName) = '' then
    Exit;
  XlsEndStream(FStream);
  FStream.SaveToFile(FileName);
  if FileExists(FileName) and FADOCompatible then 
  begin
    try
      try
        if ExtractFilePath(FileName) = '' then
          TargetFileName := IncludeTrailingBackslash(GetCurrentDir) + FileName
        else
          TargetFileName := FileName;
          
        // 创建Excel对象，需要Excel安装支持，如无安装，拦截异常
        ExcelApp := CreateOleObject( 'Excel.Application' );
        // 隐藏Excel提示(覆盖文件)
        ExcelApp.DisplayAlerts := False;
        ExcelWork := ExcelApp.WorkBooks.Open(TargetFileName);
        ExcelWork.SaveAs(TargetFileName);
        ExcelWork.Close;
        ExcelApp.Quit;
      except
      end;
    finally
      ExcelWork := Unassigned;
      ExcelApp := Unassigned;
    end;
  end;
end;

procedure TCnXlsWriter.NewXls;
begin
  FStream.Size := 0;
  XlsBeginStream(FStream, 0);
end;

procedure TCnXlsWriter.SetCells(const ACol: Byte; const ARow: Word; const Value: Variant);
var
  aStr: string;
  aInt: Int64;
  aFloat: Extended;
  aCode: Integer;
begin
  case VarType(Value) of
    varSmallint, varInteger, varByte:
      XlsWriteCellRk(FStream, ACol, ARow, Value);
    varSingle, varDouble, varCurrency:
      XlsWriteCellNumber(FStream, ACol, ARow, Value);
    varString, {$IFDEF DELPHI2009_UP} varUString, {$ENDIF} varOleStr:
      begin
        aStr := Value;
        Val(aStr, aInt, aCode);
        if aCode = 0 then 
        begin
          XlsWriteCellRk(FStream, ACol, ARow, Value);
          Exit;
        end;
        Val(aStr, aFloat, aCode);
        if aCode = 0 then 
        begin
          XlsWriteCellNumber(FStream, ACol, ARow, Value);
          Exit;
        end;
        XlsWriteCellLabel(FStream, ACol, ARow, Value);
      end;
    varDate:
      XlsWriteCellLabel(FStream, ACol, ARow, DateTimeToStr(Value));
  else
    XlsWriteCellBlank(FStream, ACol, ARow);
  end;
end;

end.
