{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDHibernateImport;
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准控件库
* 单元名称：数据导入控件单元
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
*
*             数据导入映射说明
* 一、数学表达式
*	  直接写出数学表达式即可，可以用字段作运算：
*   例如：[Area] + 1
* 二、字符串表达式
*	  用单引号引起字符串即可，例如：'abc'
* 三、GUID 表达式
*	  GUID 有读写２种特性，对于首次生成的 GUID，
*   使用写表达式，例如：GUID(w1)
*	  其中 w1 即表明写入序号为 1 的 GUID，当引用
*   GUID 时，使用读表达式，例如：
*   GUID(r1)，此时系统会从 GUID(w1)中取出值，
*   前提条件是，GUID(w1)必须存在，才能使用
*   GUID(r1)，如果有多个 GUID，依次写入
*   GUID(w2)，GUID(r2)等
* 四、合并表达式
*	  可将 xml 或 excel 中的字符直接运算，然后得
*   到的结果存入数据库。合并表达式必须针对 xml
*   或 excel 中的字段名，书写方法为方括号加 X-
*   加字段名，例如：[X-CityNo]+[X-ProvinceNo]。
* 五、条件表达式
*	  条件表达式允许用户使用 if 语句作条件判断，
*   语法如下：
*	  If([X-No]='Y'):1;
*	  If([X-No]='N'):0;
*	  语句必须用分号结尾，如果某个条件不满足，系
*   统自动返回 NULL
*
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, DB, ADODB, ComObj, CnDHibernateConsts,
  Variants, CnDHibernateBase, CnDHibernateUtils, StrUtils;

type
  TCnOnImport = procedure of object;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnDHibernateImport = class(TComponent)
  private
    FColumnLine: integer;
    FSkipHead: integer;
    FFileName: string;
    FSheetName: string;
    FMap: TStringList;
    FTableName: string;
    FConnection: TADOConnection;
    FOnImport: TCnOnImport;
    FADOTable: TADOTable;
    FAbout: string;
    function GetMap: TStrings;
    procedure SetMap(const Value: TStrings);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Import: Integer;            // 返回导入数据条数
  published
    property About: string read FAbout write FAbout;
    { excel file name }
    property FileName: string read FFileName write FFileName;
    { excel sheet name }
    property SheetName: string read FSheetName write FSheetName;
    { 跳过头部的几行？ }
    property SkipHead: integer read FSkipHead write FSkipHead default 1;
    { 列名在第几行？ }
    property ColumnLine: integer read FColumnLine write FColumnLine default 1;
    { excel 列与数据表字段的映射 }
    property Map: TStrings read GetMap write SetMap;
    { connection }
    property Connection: TADOConnection read FConnection write FConnection;
    { table name }
    property TableName: string read FTableName write FTableName;
    { on import }
    property OnImport: TCnOnImport read FOnImport write FOnImport;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateImport }

constructor TCnDHibernateImport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FMap := TStringList.Create;
  FSkipHead := 1;
  FColumnLine := 1;
end;

destructor TCnDHibernateImport.Destroy;
begin
  FConnection := nil;
  FMap.Free;
  inherited Destroy;
end;

function TCnDHibernateImport.GetMap: TStrings;
begin
  Result := FMap;
end;

function TCnDHibernateImport.Import: Integer;
var
  Excel: OleVariant;
  RowCnt: integer;
  I, J: Integer;
  HMap: ICnMap;
  N, V: string;
  Cell: string;
  Guid, G: string;
  iv: string;
begin
  Result := 0;
  // import data
  if FMap.Count = 0 then
    raise TCnNoMappingException.Create('No mapping data found!');
  if FConnection = nil then
    raise TCnNoConnectionException.Create('No connection found!');
  if FTableName = EmptyStr then
    raise TCnNoTableException.Create('No table name found!');
  if FFileName = EmptyStr then
    raise TCnNoFileException.Create('No excel file found!');
  try
    Excel := CreateOleObject('excel.application');
    Excel.WorkBooks.Open(fFileName);
  except
    raise TCnNoExcelException.Create('Excel not installed!');
    Exit;
  end;
  // get row count
  if FSheetName = Emptystr then
    raise TCnNoSheetNameException.Create('No sheet name found!');
  RowCnt := Excel.WorkSheets[FSheetName].UsedRange.Rows.Count;
  Excel.WorkSheets[FSheetName].Activate;

  // create the adotable instance
  FADOTable := TADOTable.Create(nil);
  FADOTable.Connection := FConnection;
  FADOTable.TableName := FTableName;
  FADOTable.Open;
  // formatter of FMap is
  // FieldName=ColumnNumber
  // e.g.
  // CountryName=1
  // others:
  // FieldName=Expression
  // e.g.
  // InDate=GetDate()
  HMap := StringMapToHashMap(TStringList(FMap));
  for I := FSkipHead + 1 to RowCnt do
  begin
    FADOTable.Append;
    for J := 0 to HMap.size - 1 do
    begin
      N := HMap.gettable(J).hashName;
      V := HMap.getTable(J).hashValue;
      if StrToIntDef(V, -1) = -1 then
      begin
        // expression
        if V = 'GetDate()' then
        begin
          // 取日期
          FADOTable.FieldByName(N).Value := Now;
        end
        else if Pos('GUID', V) > 0 then
        begin
          Guid := GenerateGUID;
          G := GuidRW(V, Guid, I);
          if G = 'w' then
            FADOTable.FieldByName(N).Value := Guid
          else
            FADOTable.FieldByName(N).Value := G;
            // writeLog(table.TableName + ':' + guid + '-' + g);
        end
        else if Pos('select', V) > 0 then
        begin
              // formula 字段属性
          FADOTable.FieldByName(N).Value := GetFormulaValue(V, FADOTable);
        end
        else if (Pos('X-', V) > 0) and (Pos('if', V) <= 0) then
        begin
                // todo: 处理字段合并运算
          FADOTable.FieldByName(N).Value := ExcelConvert(V, Excel, I, HMap, FADOTable);
        end
        else if pos('if', V) > 0 then
        begin
                  // todo: 处理条件运算
          FADOTable.FieldByName(N).Value := ExcelEventExpressions(V, Excel, I, HMap, FADOTable);
        end
        else
        begin
                  // 取表达式
          try
            FADOTable.FieldByName(N).Value := GetExpressValue(V, FADOTable);
          except
                    // whether number?
            if (LeftStr(V, 1) = '(') and (RightStr(V, 1) = ')') then
            begin
              iv := Copy(V, 2, Length(V) - 2);
              try
                StrToInt(iv);
                FADOTable.FieldByName(N).Value := StrToInt(iv);
              except
                        // whether string
                if LeftStr(V, 2) = '(''' then
                  V := RightStr(V, Length(V) - 2);
                if RightStr(V, 2) = ''')' then
                  V := LeftStr(V, Length(V) - 2);
                FADOTable.FieldByName(N).Value := V;
              end;
            end;
          end;
        end;
      end
      else
      begin
        // field
        Cell := Excel.Cells[I, strToInt(V)].Value;
        FADOTable.FieldByName(N).Value := Variant(Cell);
      end;
    end;
    try
      FADOTable.Post;
      Inc(Result);
      if Assigned(OnImport) then
        OnImport;
    except

    end;
  end;
  FADOTable.Close;
  FADOTable.Free;
  Excel.quit;
  Excel := Unassigned;
end;

procedure TCnDHibernateImport.SetMap(const Value: TStrings);
begin
  FMap.Assign(Value);
end;

{$ENDIF SUPPORT_ADO}
end.
