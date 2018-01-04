{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnExcelUnit;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：Excel 交互功能实现单元
* 单元作者：佚名
* 备    注：
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2003.xx.xx V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, OleServer, Variants, Excel2000, Clipbrd, Dialogs, Printers;

const
  AddrsEachRange: Integer = 255 div 16;

type
  TExcelSheet = class;
  TCellFont = class;
  TCellBorder = class;
  TPageSetup = class;
  TCellFormula = class;
  TCell = class;
  TCellAddrList = class;
  TDoubleValueList = class;
  TStringValueList = class;
  TRowHeight = class;
  TColumnWidth = class;
  TIntegerValueList = class;
  TCellValueList = class;
  THPageBreak = class;
  TLongWordValueList = class;
  TCellMerge = class;
  TCellWrapText = class;
  TInterior = class;
  TBooleanValueList = class;

  PtagCellVCol = ^tagCellVCol;
  tagCellVCol = record
    Value: string;
    Next: PtagCellVCol;
    Col: Integer;
  end;

  PtagCellVRow = ^tagCellVRow;
  tagCellVRow = record
    Next: PtagCellVRow;
    Row: Integer;
    ColHeader: tagCellVCol;
  end;

  PtagCellAddr = ^tagCellAddr;
  tagCellAddr = record
    RowT: Integer;
    ColL: Integer;
    RowB: Integer;
    ColR: Integer;
  end;

  TCellValueList = class
  private
    FValueList: TList;
  protected
    // 设置某个单元地址对应的值。
    //
    // 1.若在所有值链的地址链中找到与给定的单元地址相同的，则在该值中的地址链中删除该单元地址。
    //
    // 2.若该值已存在，则把单元地址直接加到该值的地址链中，并把AValue指向的空间释放，然后使重新指向值链中该值的地址，返回1。
    //
    // 3.若该值不存在，则添加该新值到值链中，并将该单元地址添加到新值的地址链中，返回0。
    function SetVA
      (var AValue: Pointer;
      ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0): Integer; overload;
    function IsEqual
      (A: Pointer;
      B: Pointer): Boolean; virtual; abstract;
    procedure FreePointer(AP: Pointer); virtual; abstract;

    // 删除第AIndex条记录
    // 释放TCellAddrList.Value指向的空间
    // 调用TCellAddrList.Destroy
    procedure Delete(AIndex: Integer);
  public
    function GetCount: Integer;
    destructor Destroy; override;
    constructor Create;
    function GetAddrCount(AIndex: Integer): Integer;
    function GetAddrStr
      (AIndex: Integer;
      AFrom: Integer;
      ATo: Integer): string;
    {$WARNINGS OFF}
    function GetXlsRange
      (AIndex: Integer;
      ASheet: TExcelWorksheet): ExcelRange;
    function IsInAddrList
      (AIndex: Integer;
      ARow: Integer;
      ACol: Integer;
      var ACellLT: tagCellAddr): Boolean;
    function IndexFromValue(AValue: Pointer): Integer;
    {$WARNINGS ON}
  protected

    function GetAddrList(AIndex: Integer): TCellAddrList;

  end;

  // 封装EXCEL的一张工作表
  TExcelSheet = class
  private
    FPageSetup: TPageSetup;
    FCells: TCell;
    FRows: TRowHeight;
    FCols: TColumnWidth;

    function GetPageSetup: TPageSetup;
    function GetCells
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer;
      AColR: Integer): TCell;
    function GetRows(ARow: Integer): TRowHeight;
    function GetCols(ACol: Integer): TColumnWidth;
  public
    procedure Print(ASheet: TExcelWorksheet);
    destructor Destroy; override;
    constructor Create;

    property PageSetup: TPageSetup read GetPageSetup;
    property Cells[ARowT: Integer;
    AColL: Integer;
    ARowB: Integer;
    AColR: Integer]: TCell read GetCells;
    property Rows[ARow: Integer]: TRowHeight read GetRows;
    property Cols[ACol: Integer]: TColumnWidth read GetCols;
  private
    FHPageBreaks: THPageBreak;

    function GetHPageBreaks: THPageBreak;
  public
    property HPageBreaks: THPageBreak read GetHPageBreaks;
  end;

  TCellFont = class
  private
    FName: TStringValueList;
    FSize: TIntegerValueList;
    FFontStyle: TStringValueList;
    FColorIndex: TIntegerValueList;
    FBold: TBooleanValueList;
    FItalic: TBooleanValueList;
    FUnderLine: TBooleanValueList;

    procedure SetName(AValue: string);
    procedure SetSize(AValue: Integer);
    procedure SetColorIndex(AValue: Integer);
    procedure SetBold(AValue: Boolean);
    procedure SetItalic(AValue: Boolean);
    procedure SetUnderline(AValue: Boolean);
  public
    procedure SetCurAddr
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);
    procedure Print(ASheet: TExcelWorkSheet);
    constructor Create;
    destructor Destroy; override;

    property Name: string write SetName;
    property Size: Integer write SetSize;
  private
    FRowTOfCur: Integer;
    FColLOfCur: Integer;
    FRowBOfCur: Integer;
    FColROfCur: Integer;
  public
    property ColorIndex: Integer write SetColorIndex;
    property Bold: Boolean write SetBold;
    property Italic: Boolean write SetItalic;
    property UnderLine: Boolean write SetUnderline;
  end;

  TCellBorder = class
  public
    procedure SetCurAddr
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);
    procedure Print(ASheet: TExcelWorksheet);
    destructor Destroy; override;
    constructor Create;

    procedure SetCurEdge(AEdge: SmallInt);
  private
    FRowTOfCur: Integer;
    FColLOfCur: Integer;
    FRowBOfCur: Integer;
    FColROfCur: Integer;
    FEdgeOfCur: SmallInt;

    procedure SetLineStyle(AValue: Integer);
    procedure SetWeight(AValue: Integer);
    function XlsEdgeToIndex(AXlsEdge: SmallInt): SmallInt;
    function IndexToXlsEdge(AIndex: SmallInt): SmallInt;
  public
    property LineStyle: Integer write SetLineStyle;
    property Weight: Integer write SetWeight;
  private
    FEdgesLineStyle: array[0..5] of TIntegerValueList;
    FEdgesWeight: array[0..5] of TIntegerValueList;
  end;

  TPageSetup = class
  public
    LeftMargin: Double;
    RightMargin: Double;
    TopMargin: Double;
    BottomMargin: Double;
    PaperSize: Integer;
    Orientation: Integer;
    HeaderMargin: Double;
    FooterMargin: Double;
    LeftHeader: string;
    CenterHeader: string;
    RightHeader: string;
    LeftFooter: string;
    CenterFooter: string;
    RightFooter: string;

    procedure Print(ASheet: TExcelWorksheet);
    destructor Destroy; override;
    constructor Create;
  private
    EMPTY_INTEGER: Integer;
    EMPTY_DOUBLE: Double;
    EMPTY_STRING: string;
  end;

  // 对于已经合并的单元格，只能对合并单元格的左上角单元格进行设置，否则会出错
  TCellFormula = class
  private
    FFormulaArray: tagCellVRow;

    // 对于已经合并的单元格，只能对合并单元格的左上角单元格进行设置，否则会出错
    procedure SetFormula(AValue: string);
  public
    procedure Print(ASheet: TExcelWorksheet);
    procedure SetCurAddr
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);

    // 初始化，如设置FFormulaArray的Next为nil
    constructor Create;

    // 释放空间，如释放FFormulaArray中所占的空间
    destructor Destroy; override;

    property Formula: string write SetFormula;
  private
    FRowTOfCur: Integer;
    FColLOfCur: Integer;
    FRowBOfCur: Integer;
    FColROfCur: Integer;
  end;

  TCell = class
  private
    FFormula: TCellFormula;
    FBorders: TCellBorder;
    FInterior: TInterior;
    FWrapText: TCellWrapText;

    function GetFont: TCellFont;
    function GetBorders(AEdge: SmallInt): TCellBorder;

    // 对于已经合并的单元格，只能对合并单元格的左上角单元格进行设置，否则会出错
    procedure SetFormula(AValue: Variant);
    procedure SetWrapText(AValue: Boolean);
    function GetInterior: TInterior;
  public
    // 设置当前的需要设置的单元地址
    procedure SetCurAddr
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);
    procedure Print(ASheet: TExcelWorksheet);

    property Formula: Variant write SetFormula;
    property Font: TCellFont read GetFont;
    property Borders[AEdge: SmallInt]: TCellBorder read GetBorders;
    property WrapText: Boolean write SetWrapText;

    destructor Destroy; override;
    constructor Create;

    procedure FullLineBTLRVH(ALineStyle: Integer);
  private
    FRowTOfCur: Integer;
    FColLOfCur: Integer;
    FRowBOfCur: Integer;
    FColROfCur: Integer;

    procedure SetMerge(AValue: Boolean);
  public

    property Merge: Boolean write SetMerge;
  private
    FFonts: TCellFont;
    FMerges: TCellMerge;

    procedure SetNumberFormatLocal(AValue: string);
  public
    property NumberFormatLocal: string write SetNumberFormatLocal;
  private
    FNumberFormatLocal: TStringValueList;
    FHorizontalAlignment: TLongWordValueList;
    FVerticalAlignment: TLongWordValueList;

    procedure SetHorizontalAlignment(AValue: LongWord);
    procedure SetVerticalAlignment(AValue: LongWord);
  public
    property HorizontalAlignment: LongWord write SetHorizontalAlignment;
    property VerticalAlignment: LongWord write SetVerticalAlignment;
    property Interior: TInterior read GetInterior;

  end;

  TCellAddrList = class
  private
    FAddrList: TList;
    FValue: Pointer;

    // 把PtagCellAddr指向的单元地址转换成字符串的形式
    function AddrToStr(PCellAddr: PtagCellAddr): string;
  public
    // 添加一个单元地址
    // ARow及ACol必需>=1
    // 创建一个tagCellAddr入FAddrList
    procedure Add
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);

    // 去除一个单元
    // ARow及ACol应该是其它单元中涉及到的单元格
    procedure Delete
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);

    // 对所有成员进行初始化，如FAddrList
    constructor Create;

    // 安全清除FAddrList所占的内存
    destructor Destroy; override;

    // 返回从AFrom到ATo之间的地址组合的单元引用字符串
    function GetAddrStr
      (AFrom: Integer;
      ATo: Integer): string;

    // 返回该值对应的单元的总数
    function GetAddrCount: Integer;

    property Value: Pointer read FValue write FValue;

    // 判断[ABegin1..AEnd1]和[ABegin2..AEnd2]是否有交集
    // 有则返回True,ABeginOut和AEndOut返回交集的上下界
    // 否则返回False,ABeginOut和AEndOut不确定。
    function IsInterSection
      (ABegin1: Integer;
      AEnd1: Integer;
      ABegin2: Integer;
      AEnd2: Integer;
      var ABeginOut: Integer;
      out AEndOut: Integer): Boolean;
    {$WARNINGS OFF}
    function GetXlsRange(ASheet: TExcelWorksheet): ExcelRange;

    // 若ARow,ACol在地址链中，返回True,否则返回False
    function IsInAddrList
      (ARow: Integer;
      ACol: Integer;
      var ACellLT: tagCellAddr): Boolean;
    {$WARNINGS ON}
  end;

  TDoubleValueList = class(TCellValueList)
  public
    // 设置某个单元地址对应的值。
    //
    // 1.若在所有值链的地址链中找到与给定的单元地址相同的，则在该值中的地址链中删除该单元地址。
    //
    // 2.若该值已存在，则把单元地址直接加到该值的地址链中，并把AValue指向的空间释放，然后使重新指向值链中该值的地址，返回1。
    //
    // 3.若该值不存在，则添加该新值到值链中，并将该单元地址添加到新值的地址链中，返回0。
    function SetVA
      (AValue: Double;
      ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0): Integer; overload;
  protected
    // 将AP转化成Double的类型，并释放空间
    procedure FreePointer(AP: Pointer); override;

    // 将A，B转化成Double的类型，并比较A，B指向的值是否相等，如果相等返回True,否则返回False
    function IsEqual
      (A: Pointer;
      B: Pointer): Boolean; override;
  private
    // 将值链中某个Pointer转化成Double型，并返回
    function GetValue(AIndex: Integer): Double;
  public
    property Value[AIndex: Integer]: Double read GetValue;
  end;

  TStringValueList = class(TCellValueList)
  public
    // 设置某个单元地址对应的值。
    //
    // 1.若在所有值链的地址链中找到与给定的单元地址相同的，则在该值中的地址链中删除该单元地址。
    //
    // 2.若该值已存在，则把单元地址直接加到该值的地址链中，并把AValue指向的空间释放，然后使重新指向值链中该值的地址，返回1。
    //
    // 3.若该值不存在，则添加该新值到值链中，并将该单元地址添加到新值的地址链中，返回0。
    function SetVA
      (AValue: string;
      ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0): Integer; overload;
  protected
    // 将AP转化成String的类型，并释放空间
    procedure FreePointer(AP: Pointer); override;

    // 将A，B转化成String的类型，并比较A，B指向的值是否相等，如果相等返回True,否则返回False
    function IsEqual
      (A: Pointer;
      B: Pointer): Boolean; override;
  private
    // 将值链中某个Pointer转化成String型，并返回
    function GetValue(AIndex: Integer): string;
  public
    property Value[AIndex: Integer]: string read GetValue;
  end;

  TRowHeight = class
  private
    FRowOfCur: Integer;
    FRow: TDoubleValueList;

    procedure SetHeight(AValue: Double);
  public
    procedure Print(ASheet: TExcelWorksheet);
    procedure SetCurRow(ARow: Integer);
    destructor Destroy; override;
    constructor Create;

    property Height: Double write SetHeight;
  end;

  TColumnWidth = class
  private
    FColOfCur: Integer;
    FCol: TDoubleValueList;

    procedure SetWidth(AValue: Double);
  public
    procedure Print(ASheet: TExcelWorksheet);
    procedure SetCurCol(ACol: Integer);
    destructor Destroy; override;
    constructor Create;

    property Width: Double write SetWidth;
  end;

  TIntegerValueList = class(TCellValueList)
  public
    // 设置某个单元地址对应的值。
    //
    // 1.若在所有值链的地址链中找到与给定的单元地址相同的，则在该值中的地址链中删除该单元地址。
    //
    // 2.若该值已存在，则把单元地址直接加到该值的地址链中，并把AValue指向的空间释放，然后使重新指向值链中该值的地址，返回1。
    //
    // 3.若该值不存在，则添加该新值到值链中，并将该单元地址添加到新值的地址链中，返回0。
    function SetVA
      (AValue: Integer;
      ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0): Integer; overload;
  protected
    // 将AP转化成Inetger的类型，并释放空间
    procedure FreePointer(AP: Pointer); override;

    // 将A，B转化成Integer的类型，并比较A，B指向的值是否相等，如果相等返回True,否则返回False
    function IsEqual
      (A: Pointer;
      B: Pointer): Boolean; override;
  private
    // 将值链中某个Pointer转化成Integer型，并返回
    function GetValue(AIndex: Integer): Integer;
  public
    property Value[AIndex: Integer]: Integer read GetValue;
  end;

  THPageBreak = class
  private
    FHPageBreak: TIntegerValueList;
  public
    procedure Print(ASheet: TExcelWorksheet);
    procedure AddBefore(ARowBefore: Integer);
    destructor Destroy; override;
    constructor Create;
  end;

  TLongWordValueList = class(TCellValueList)
  public
    // 设置某个单元地址对应的值。
    //
    // 1.若在所有值链的地址链中找到与给定的单元地址相同的，则在该值中的地址链中删除该单元地址。
    //
    // 2.若该值已存在，则把单元地址直接加到该值的地址链中，并把AValue指向的空间释放，然后使重新指向值链中该值的地址，返回1。
    //
    // 3.若该值不存在，则添加该新值到值链中，并将该单元地址添加到新值的地址链中，返回0。
    function SetVA
      (AValue: LongWord;
      ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0): Integer; overload;
  protected
    // 将A，B转化成LongWord的类型，并比较A，B指向的值是否相等，如果相等返回True,否则返回False
    function IsEqual
      (A: Pointer;
      B: Pointer): Boolean; override;

    // 将AP转化成LongWord的类型，并释放空间
    procedure FreePointer(AP: Pointer); override;
  private
    // 将值链中某个Pointer转化成LongWord型，并返回
    function GetValue(AIndex: Integer): LongWord;
  public
    property Value[AIndex: Integer]: LongWord read GetValue;
  end;

  TCellMerge = class
  private
    FRowTOfCur: Integer;
    FColLOfCur: Integer;
    FRowBOfCur: Integer;
    FColROfCur: Integer;
    FMerge: TBooleanValueList;

    procedure SetMerge(AValue: Boolean);
  public
    procedure Print(ASheet: TExcelWorksheet);
    procedure SetCurAddr
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);
    destructor Destroy; override;
    constructor Create;
    function IsMerged
      (ARow: Integer;
      ACol: Integer;
      var ACellLT: tagCellAddr): Boolean;

    property Merge: Boolean write SetMerge;
  end;

  TCellWrapText = class
  private
    FRowTOfCur: Integer;
    FColLOfCur: Integer;
    FRowBOfCur: Integer;
    FColROfCur: Integer;
    FWrapText: TBooleanValueList;

    procedure SetWrapText(AValue: Boolean);
  public
    procedure Print(ASheet: TExcelWorksheet);
    procedure SetCurAddr
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);
    destructor Destroy; override;
    constructor Create;

    property WrapText: Boolean write SetWrapText;
  end;

  TInterior = class
  private
    FRowTOfCur: Integer;
    FColLOfCur: Integer;
    FRowBOfCur: Integer;
    FColROfCur: Integer;
    FColorIndex: TIntegerValueList;

    procedure SetColorIndex(AColorIndex: Integer);
  public
    procedure SetCurAddr
      (ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0);
    destructor Destroy; override;
    constructor Create;
    procedure Print(ASheet: TExcelWorksheet);

    property ColorIndex: Integer write SetColorIndex;
  end;

  TBooleanValueList = class(TCellValueList)
  public
    function SetVA
      (AValue: Boolean;
      ARowT: Integer;
      AColL: Integer;
      ARowB: Integer = 0;
      AColR: Integer = 0): Integer; overload;
  protected
    procedure FreePointer(AP: Pointer); override;
    function IsEqual
      (A: Pointer;
      B: Pointer): Boolean; override;
  private
    function GetValue(AIndex: Integer): Boolean;
  public
    property Value[AIndex: Integer]: Boolean read GetValue;
  end;

implementation

procedure SwapInt
  (var A: Integer;
  var B: Integer);
var
  Temp: Integer;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

procedure VerifyAndFixAddrParam
  (var ARowT: Integer;
  var AColL: Integer;
  var ARowB: Integer;
  var AColR: Integer);
begin
  Assert((ARowT > 0) and (AColL > 0), '地址设置时出错，地址应该大于等于1');

  if ARowB = 0 then
  begin
    Assert(AColR = 0, '地址设置时出错，缺少一个参数');
    ARowB := ARowT;
    AColR := AColL;
  end else
    Assert((ARowB > 0) and (AColR > 0), '地址设置时出错，地址应该大于等于1');

  if ARowT > ARowB then SwapInt(ARowT, ARowB);
  if AColL > AColR then SwapInt(AColL, AColR);

end;

procedure TExcelSheet.Print(ASheet: TExcelWorksheet);
begin
//  ASheet.Visible[0]:= xlSheetHidden;
  FPageSetup.Print(ASheet);
  FHPageBreaks.Print(ASheet);
  FRows.Print(ASheet);
  FCols.Print(ASheet);
  FCells.Print(ASheet);
  ASheet.Visible[0] := xlSheetVisible;
  ASheet.SELECT;
  ASheet.Range['A1', EMPTYPARAM].SELECT;
end;

destructor TExcelSheet.Destroy;
begin
  FPageSetup.Destroy;
  FCells.Destroy;
  FCols.Destroy;
  FRows.Destroy;
  FHPageBreaks.Destroy;
end;

constructor TExcelSheet.Create;
begin
  FCells := TCell.Create;
  FCols := TColumnWidth.Create;
  FRows := TRowHeight.Create;
  FPageSetup := TPageSetup.Create;
  FHPageBreaks := THPageBreak.Create;
end;

function TExcelSheet.GetPageSetup: TPageSetup;
begin
  Result := FPageSetup;
end;

function TExcelSheet.GetCells
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer): TCell;
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);
  FCells.SetCurAddr(ARowT, AColL, ARowB, AColR);
  Result := FCells;
end;

function TExcelSheet.GetRows(ARow: Integer): TRowHeight;
begin
  FRows.SetCurRow(ARow);
  Result := FRows;
end;

function TExcelSheet.GetCols(ACol: Integer): TColumnWidth;
begin
  FCols.SetCurCol(ACol);
  Result := FCols;
end;

procedure TCellFont.SetName(AValue: string);
begin
  FName.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCellFont.SetSize(AValue: Integer);
begin
  FSize.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCellFont.SetCurAddr
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer);
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  FRowTOfCur := ARowT;
  FColLOfCur := AColL;
  FRowBOfCur := ARowB;
  FColROfCur := AColR;

end;

procedure TCellFont.Print(ASheet: TExcelWorkSheet);
var
  i: Integer;
begin
  {输出Name}
  with FName do
    for i := 0 to GetCount() - 1 do
      if GetAddrCount(i) > 0 then
        GetXlsRange(i, ASheet).Font.Name := Value[i];

  {输出Size}
  with FSize do
    for i := 0 to GetCount() - 1 do
      if GetAddrCount(i) > 0 then
        GetXlsRange(i, ASheet).Font.Size := Value[i];

  {输出ColorIndex}
  with FColorIndex do
    for i := 0 to GetCount() - 1 do
      if GetAddrCount(i) > 0 then
        GetXlsRange(i, ASheet).Font.ColorIndex := Value[i];

  {输出Bold}
  with FBold do
    for i := 0 to GetCount() - 1 do
      if GetAddrCount(i) > 0 then
        GetXlsRange(i, ASheet).Font.Bold := Value[i];

  {输出Italic}
  with FItalic do
    for i := 0 to GetCount() - 1 do
      if GetAddrCount(i) > 0 then
        GetXlsRange(i, ASheet).Font.Italic := Value[i];

  {输出Underline}
  with FUnderLine do
    for i := 0 to GetCount() - 1 do
      if GetAddrCount(i) > 0 then
        GetXlsRange(i, ASheet).Font.UnderLine := Value[i];

end;

constructor TCellFont.Create;
begin
  FName := TStringValueList.Create;
  FSize := TIntegerValueList.Create;
  FFontStyle := TStringValueList.Create;
  FColorIndex := TIntegerValueList.Create;
  FBold := TBooleanValueList.Create;
  FItalic := TBooleanValueList.Create;
  FUnderLine := TBooleanValueList.Create;
end;

destructor TCellFont.Destroy;
begin
  FSize.Destroy;
  FName.Destroy;
  FColorIndex.Destroy;
  FFontStyle.Destroy;
  FBold.Destroy;
  FItalic.Destroy;
  FUnderLine.Destroy;
end;

procedure TCellFont.SetColorIndex(AValue: Integer);
begin
  FColorIndex.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCellFont.SetBold(AValue: Boolean);
begin
  FBold.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCellFont.SetItalic(AValue: Boolean);
begin
  FItalic.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCellFont.SetUnderline(AValue: Boolean);
begin
  FUnderLine.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCellBorder.SetCurAddr
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer);
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  FRowTOfCur := ARowT;
  FColLOfCur := AColL;
  FRowBOfCur := ARowB;
  FColROfCur := AColR;

end;

procedure TCellBorder.Print(ASheet: TExcelWorksheet);
var
  i: Integer;
  j: Integer;
  k: Integer;
begin
  {输出EdgesLineStyle}
  for i := Low(FEdgesLineStyle) to High(FEdgesLineStyle) do
  begin
    with FEdgesLineStyle[i] do
      for j := 0 to GetCount - 1 do
        if GetAddrCount(j) > 0 then
          for k := 0 to (GetAddrCount(j) div AddrsEachRange) do
            ASheet.Range[GetAddrStr(j, k * AddrsEachRange, (k + 1) * AddrsEachRange -
              1), EmptyParam].Borders[IndexToXlsEdge(i)].LineStyle := Value[j];
  end;

  {输出EdgesWeight}
  for i := Low(FEdgesLineStyle) to High(FEdgesLineStyle) do
  begin
    with FEdgesWeight[i] do
      for j := 0 to GetCount - 1 do
        if GetAddrCount(j) > 0 then
          for k := 0 to (GetAddrCount(j) div AddrsEachRange) do
            ASheet.Range[GetAddrStr(j, k * AddrsEachRange, (k + 1) * AddrsEachRange -
              1), EmptyParam].Borders[IndexToXlsEdge(i)].Weight := Value[j];
  end;

end;

destructor TCellBorder.Destroy;
var
  i: Integer;
begin
  for i := Low(FEdgesLineStyle) to High(FEdgesLineStyle) do
  begin
    FEdgesLineStyle[i].Destroy;
  end;
  for i := Low(FEdgesWeight) to High(FEdgesWeight) do
  begin
    FEdgesWeight[i].Destroy;
  end;
end;

constructor TCellBorder.Create;
var
  i: Integer;
begin
  for i := Low(FEdgesLineStyle) to High(FEdgesLineStyle) do
  begin
    FEdgesLineStyle[i] := TIntegerValueList.Create;
    FEdgesWeight[i] := TIntegerValueList.Create;
  end;
end;

procedure TPageSetup.Print(ASheet: TExcelWorksheet);
begin

  if Printer.Printers.Count = 0 then
  begin
    ShowMessage('该计算机中未安装任何打印机的驱动程序，请首先安装打印机的驱动程序方可设置页边距和纸张大小等'#13'There isn''t any printer''s driver on this computer. Please install one first');
    Exit;
  end;

  try

    if BottomMargin <> EMPTY_DOUBLE then ASheet.PageSetup.BottomMargin := BottomMargin;
    if LeftMargin <> EMPTY_DOUBLE then ASheet.PageSetup.LeftMargin := LeftMargin;
    if TopMargin <> EMPTY_DOUBLE then ASheet.PageSetup.TopMargin := TopMargin;
    if RightMargin <> EMPTY_DOUBLE then ASheet.PageSetup.RightMargin := RightMargin;
    if HeaderMargin <> EMPTY_DOUBLE then ASheet.PageSetup.HeaderMargin := HeaderMargin;
    if FooterMargin <> EMPTY_DOUBLE then ASheet.PageSetup.FooterMargin := FooterMargin;

  except
    ShowMessage('默认的打印机不支持该页边距或面眉面脚边距设置'#13'The default printer doesn''t support these paper margins or header footer margin');
  end;

  try

    if Orientation <> EMPTY_INTEGER then ASheet.PageSetup.Orientation := Orientation;
    if PaperSize <> EMPTY_INTEGER then ASheet.PageSetup.PaperSize := PaperSize;

  except
    ShowMessage('默认的打印机不支持该纸张大小或打印方向'#13'The default printer doesn''t support this paper size or printer orientation');
  end;

  try

    if LeftFooter <> EMPTY_STRING then ASheet.PageSetup.LeftFooter := LeftFooter;
    if CenterFooter <> EMPTY_STRING then ASheet.PageSetup.CenterFooter := CenterFooter;
    if RightFooter <> EMPTY_STRING then ASheet.PageSetup.RightFooter := RightFooter;
    if LeftHeader <> EMPTY_STRING then ASheet.PageSetup.LeftHeader := LeftHeader;
    if CenterHeader <> EMPTY_STRING then ASheet.PageSetup.CenterHeader := CenterHeader;
    if RightHeader <> EMPTY_STRING then ASheet.PageSetup.RightHeader := RightHeader;

  except
    ShowMessage('默认的打印机不支持页眉页脚的字符串设置'#13'The default printer doesn''t support the footer or header');
  end;

end;

destructor TPageSetup.Destroy;
begin
end;

constructor TPageSetup.Create;
begin
  {初始化空标志}
  EMPTY_INTEGER := -999;
  EMPTY_DOUBLE := -999;
  EMPTY_STRING := '';

  {初始化各变量}
  BottomMargin := EMPTY_DOUBLE;
  LeftMargin := EMPTY_DOUBLE;
  TopMargin := EMPTY_DOUBLE;
  RightMargin := EMPTY_DOUBLE;
  FooterMargin := EMPTY_DOUBLE;
  HeaderMargin := EMPTY_DOUBLE;

  PaperSize := EMPTY_INTEGER;
  Orientation := EMPTY_INTEGER;

  LeftHeader := EMPTY_STRING;
  CenterHeader := EMPTY_STRING;
  RightHeader := EMPTY_STRING;
  LeftFooter := EMPTY_STRING;
  CenterFooter := EMPTY_STRING;
  RightFooter := EMPTY_STRING;

end;

procedure TCellFormula.Print(ASheet: TExcelWorksheet);
var
  PCellVRow: PtagCellVRow;
  PCellVCol: PtagCellVCol;
  Str: string;
  RowPrev, ColPrev: Integer;
  i: Integer;
begin
  Str := '';
  PCellVRow := FFormulaArray.Next;
  RowPrev := 1;
  while PCellVRow <> nil do
  begin
    for i := 0 to PCellVRow^.Row - RowPrev - 1 do
      Str := Str + #13 + #10;
    RowPrev := PCellVRow^.Row;
    PCellVCol := PCellVRow^.ColHeader.Next;
    ColPrev := 1;
    while PCellVCol <> nil do
    begin
      for i := 0 to PCellVCol^.Col - ColPrev - 1 do
        Str := Str + #9;
      ColPrev := PCellVCol^.Col;
      Str := Str + PCellVCol^.Value;
      PCellVCol := PCellVCol^.Next;
    end;
    PCellVRow := pCellVRow^.Next;
  end;
  clipboard.AsText := Str;
  try
    ASheet.Range['A1', EMPTYPARAM].SELECT;
    ASheet.Paste();
  except
    MessageDlg('对Formula设置错误。请注意对于合并单元格只能对左上角的单元格的Formula进行设置', mtError, [mbOK], 0);
  end;
//  ClipBoard.Clear;
end;

procedure TCellFormula.SetFormula(AValue: string);
var
  PCellVRow, PRowPrev, PNewVRow: PtagCellVRow;
  PCellVCol, PColPrev, PNewVCol: PtagCellVCol;
  Sign: Boolean;
  Row, Col: Integer;
begin
  for Row := FRowTOfCur to FRowBOfCur do
    for Col := FColLOfCur to FColROfCur do
    begin
    {在链中寻找行}
      PCellVRow := FFormulaArray.Next;
      PRowPrev := Addr(FFormulaArray);
      Sign := False;
      while PCellVRow <> nil do
      begin
        if PCellVRow^.Row = Row then
        begin
          Sign := True;
          Break;
        end;
        if PCellVRow^.Row > Row then Break;
        PRowPrev := PCellVRow;
        PCellVRow := PCellVRow^.Next;
      end;
    {若没有该行，则添加该行到链中}
      if not Sign then
      begin
        New(PNewVRow);
        PNewVRow^.ColHeader.Next := nil;
        PNewVRow^.Row := Row;
        PNewVRow^.Next := PCellVRow;
        PRowPrev^.Next := PNewVRow;
        PCellVRow := PNewVRow;
      end;

    {在链中寻找列}
      PCellVCol := PCellVRow^.ColHeader.Next;
      PColPrev := Addr(PCellVRow^.ColHeader);
      Sign := False;
      while PCellVCol <> nil do
      begin
        if PCellVCol^.Col = Col then
        begin
          Sign := True;
          Break;
        end;
        if PCellVCol^.Col > Col then Break;
        PColPrev := PCellVCol;
        PCellVCol := PCellVCol^.Next;
      end;
    {若在链中没有该列，则添加该列}
      if not Sign then
      begin
        New(PNewVCol);
        PNewVCol^.Col := Col;
        PNewVCol^.Value := '';
        PNewVCol^.Next := PCellVCol;
        PColPrev^.Next := PNewVCol;
        PCellVCol := PNewVCol;
      end;

      PCellVCol^.Value := AValue;
    end;
end;

procedure TCellFormula.SetCurAddr
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer);
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  FRowTOfCur := ARowT;
  FColLOfCur := AColL;
  FRowBOfCur := ARowB;
  FColROfCur := AColR;

end;

constructor TCellFormula.Create;
begin
  FFormulaArray.Next := nil;
end;

destructor TCellFormula.Destroy;
var
  PCellVRow, RowTemp: PtagCellVRow;
  PCellVCol, ColTemp: PtagCellVCol;
begin
  PCellVRow := FFormulaArray.Next;
  while PCellVRow <> nil do
  begin
    PCellVCol := PCellVRow^.ColHeader.Next;
    while PCellVCol <> nil do
    begin
      ColTemp := PCellVCol;
      PCellVCol := PCellVCol^.Next;
      Dispose(ColTemp);
    end;
    RowTemp := PCellVRow;
    PCellVRow := PCellVRow^.Next;
    Dispose(RowTemp);
  end;
end;

procedure TCell.SetCurAddr
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer);
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  FRowTOfCur := ARowT;
  FColLOfCur := AColL;
  FRowBOfCur := ARowB;
  FColROfCur := AColR;

end;

procedure TCell.Print(ASheet: TExcelWorksheet);
var
  i: Integer;
begin
  FFonts.Print(ASheet);

  {输出NumberFormatLocal}
  for i := 0 to FNumberFormatLocal.GetCount - 1 do
    if FNumberFormatLocal.GetAddrCount(i) > 0 then
      FNumberFormatLocal.GetXlsRange(i, ASheet).NumberFormatLocal :=
        FNumberFormatLocal.Value[i];

  FFormula.Print(ASheet);

  FMerges.Print(ASheet);

  FBorders.Print(ASheet);

  {输出HorizontalAlignment}
  for i := 0 to FHorizontalAlignment.GetCount - 1 do
    if FHorizontalAlignment.GetAddrCount(i) > 0 then
      FHorizontalAlignment.GetXlsRange(i, ASheet).HorizontalAlignment :=
        FHorizontalAlignment.Value[i];

  {输出VerticalAlignment}
  for i := 0 to FVerticalAlignment.GetCount - 1 do
    if FVerticalAlignment.GetAddrCount(i) > 0 then
      FVerticalAlignment.GetXlsRange(i, ASheet).VerticalAlignment :=
        FVerticalAlignment.Value[i];

  FInterior.Print(ASheet);

  FWrapText.Print(ASheet);
end;

function TCell.GetFont: TCellFont;
begin
  FFonts.SetCurAddr(FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
  Result := FFonts;
end;

function TCell.GetBorders(AEdge: SmallInt): TCellBorder;
begin
  FBorders.SetCurAddr(FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
  FBorders.SetCurEdge(AEdge);
  Result := FBorders;
end;

procedure TCellAddrList.Add
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer);
var
  PAddr: PtagCellAddr;
  i: Integer;
begin

  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  {检查地址链中是否已经有该单元了}
  i := 0;
  while i < FAddrList.Count do
  begin
    PAddr := PtagCellAddr(FAddrList.Items[i]);
    if (PAddr^.RowT <= ARowT) and (PAddr^.RowB >= ARowB) and
      (PAddr^.ColL <= AColL) and (PAddr^.ColR >= AColR) then
      Exit;

    if (ARowT <= PAddr^.RowT) and (ARowB >= PAddr^.RowB) and
      (AColL <= PAddr^.ColL) and (AColR >= PAddr^.ColR) then
    begin
      FAddrList.Delete(i);
      Dec(i);
      Dispose(PAddr);
    end;
    Inc(i);
  end;

  New(PAddr);
  PAddr^.RowT := ARowT;
  PAddr^.ColL := AColL;
  PAddr^.RowB := ARowB;
  PAddr^.ColR := AColR;
  FAddrList.Add(PAddr);
end;

procedure TCellAddrList.Delete
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer);
var
  PAddr: PtagCellAddr;
  i: Integer;
  InsectT, InsectL, InsectB, InsectR: Integer;
begin

  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  i := 0;
  while i < FAddrList.Count do
  begin
    PAddr := PtagCellAddr(FAddrList.Items[i]);
    {判断删除的单元是否和地址链中的地址有交集}
    if IsInterSection(PAddr^.RowT, PAddr^.RowB, ARowT, ARowB, InsectT, InsectB) and
      IsInterSection(PAddr^.ColL, PAddr^.ColR, AColL, AColR, InsectL, InsectR) then
    begin

      FAddrList.Delete(i);

      if InsectL > PAddr^.ColL then
        Add(PAddr^.RowT, PAddr^.ColL, PAddr^.RowB, InsectL - 1);
      if InsectR < PAddr^.ColR then
        Add(PAddr^.RowT, InsectR + 1, PAddr^.RowB, PAddr^.ColR);
      if InsectT > PAddr^.RowT then
        Add(PAddr^.RowT, InsectL, InsectT - 1, InsectR);
      if InsectB < PAddr^.RowB then
        Add(InsectB + 1, InsectL, PAddr^.RowB, InsectR);

      Dec(i);
      Dispose(PAddr);
    end;
    Inc(i);
  end;
end;

constructor TCellAddrList.Create;
begin
  FAddrList := TList.Create;
end;

destructor TCellAddrList.Destroy;
var
  i, Count: Integer;
begin
  Count := FAddrList.Count;
  for i := 0 to Count - 1 do
    Dispose(PtagCellAddr(FAddrList.Items[i]));
  FAddrList.Destroy;
end;

function TCellAddrList.GetAddrStr
  (AFrom: Integer;
  ATo: Integer): string;
var
  i: Integer;
  Temp: Integer;
begin

  Assert(GetAddrCount() > 0, '调用GetAddrStr时出错，地址链表中无地址');

  if AFrom > ATo then
  begin
    Temp := AFrom;
    AFrom := ATo;
    ATo := Temp;
  end;

  if AFrom > GetAddrCount() - 1 then AFrom := GetAddrCount() - 1;
  if AFrom < 0 then AFrom := 0;
  if ATo > GetAddrCount() - 1 then ATo := GetAddrCount() - 1;
  if ATo < 0 then ATo := 0;

  Result := AddrToStr(PtagCellAddr(FAddrList.Items[AFrom]));
  for i := AFrom + 1 to ATo do
    Result := Result + ',' + AddrToStr(PtagCellAddr(FAddrList.Items[i]));
end;

function TCellAddrList.GetAddrCount: Integer;
begin
  Result := FAddrList.Count;
end;

function TDoubleValueList.SetVA
  (AValue: Double;
  ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer): Integer;
var
  PNewValue: PDouble;
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);
  New(PNewValue);
  PNewValue^ := AValue;
  Result := SetVA(Pointer(PNewValue), ARowT, AColL, ARowB, AColR);
end;

procedure TDoubleValueList.FreePointer(AP: Pointer);
begin
  Dispose(PDouble(AP));
end;

function TDoubleValueList.IsEqual
  (A: Pointer;
  B: Pointer): Boolean;
begin
  if PDouble(A)^ = PDouble(B)^ then
    Result := True
  else
    Result := False;
end;

function TDoubleValueList.GetValue(AIndex: Integer): Double;
begin
  Result := PDouble(GetAddrList(AIndex).Value)^;
end;

function TStringValueList.SetVA
  (AValue: string;
  ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer): Integer;
var
  PNewValue: PString;
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);
  New(PNewValue);
  PNewValue^ := AValue;
  Result := SetVA(Pointer(PNewValue), ARowT, AColL, ARowB, AColR);
end;

procedure TStringValueList.FreePointer(AP: Pointer);
begin
  Dispose(PString(AP));
end;

function TStringValueList.IsEqual
  (A: Pointer;
  B: Pointer): Boolean;
begin
  if PString(A)^ = PString(B)^ then
    Result := True
  else
    Result := False;
end;

function TStringValueList.GetValue(AIndex: Integer): string;
begin
  Result := PString(GetAddrList(AIndex).Value)^;
end;

procedure TRowHeight.Print(ASheet: TExcelWorksheet);
var
  i: Integer;
begin
  {输出Row}
  for i := 0 to FRow.GetCount - 1 do
    if FRow.GetAddrCount(i) > 0 then
      FRow.GetXlsRange(i, ASheet).RowHeight := FRow.Value[i];
end;

procedure TRowHeight.SetCurRow(ARow: Integer);
begin
  FRowOfCur := ARow;
end;

procedure TRowHeight.SetHeight(AValue: Double);
begin
  FRow.SetVA(AValue, FRowOfCur, 1);
end;

destructor TRowHeight.Destroy;
begin
  FRow.Destroy;
end;

constructor TRowHeight.Create;
begin
  FRow := TDoubleValueList.Create;
end;

procedure TColumnWidth.Print(ASheet: TExcelWorksheet);
var
  i: Integer;
begin
  {输出Col}
  for i := 0 to FCol.GetCount - 1 do
    if FCol.GetAddrCount(i) > 0 then
      FCol.GetXlsRange(i, ASheet).ColumnWidth := FCol.Value[i];
end;

procedure TColumnWidth.SetCurCol(ACol: Integer);
begin
  FColOfCur := ACol;
end;

procedure TColumnWidth.SetWidth(AValue: Double);
begin
  FCol.SetVA(AValue, 1, FColOfCur);
end;

destructor TColumnWidth.Destroy;
begin
  FCol.Destroy;
end;

constructor TColumnWidth.Create;
begin
  FCol := TDoubleValueList.Create;
end;

function TIntegerValueList.SetVA
  (AValue: Integer;
  ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer): Integer;
var
  NewValue: PInteger;
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);
  New(NewValue);
  NewValue^ := AValue;
  Result := SetVA(Pointer(NewValue), ARowT, AColL, ARowB, AColR);
end;

procedure TIntegerValueList.FreePointer(AP: Pointer);
begin
  Dispose(PInteger(AP));
end;

function TIntegerValueList.IsEqual
  (A: Pointer;
  B: Pointer): Boolean;
begin
  if PInteger(A)^ = PInteger(B)^ then
    Result := True
  else
    Result := False;
end;

function TIntegerValueList.GetValue(AIndex: Integer): Integer;
begin
  Result := PInteger(GetAddrList(AIndex).Value)^;
end;

function TCellValueList.SetVA
  (var AValue: Pointer;
  ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer): Integer;
var
  i: Integer;
  CellAddrList: TCellAddrList;
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  Result := 0;
  for i := 0 to GetCount() - 1 do
  begin
    CellAddrList := GetAddrList(i);
    if IsEqual(AValue, CellAddrList.Value) then
    begin
      CellAddrList.Add(ARowT, AColL, ARowB, AColR);
      FreePointer(AValue);
      AValue := CellAddrList.Value;
      Result := 1;
    end else
      CellAddrList.Delete(ARowT, AColL, ARowB, AColR);
  end;
  if Result = 0 then
  begin
    CellAddrList := TCellAddrList.Create;
    CellAddrList.Value := AValue;
    CellAddrList.Add(ARowT, AColL, ARowB, AColR);
    FValueList.Add(CellAddrList);
  end;
end;

function TCellValueList.GetCount: Integer;
begin
  Result := FValueList.Count;
end;

destructor TCellValueList.Destroy;
begin
  while GetCount() > 0 do
    Delete(0);
  FValueList.Destroy;
end;

constructor TCellValueList.Create;
begin
  FValueList := TList.Create;
end;

procedure TCellValueList.Delete(AIndex: Integer);
var
  CellAddrList: TCellAddrList;
begin
  CellAddrList := GetAddrList(AIndex);
  FreePointer(CellAddrList.Value);
  CellAddrList.Destroy;
  FValueList.Delete(AIndex);
end;

function TCellValueList.GetAddrCount(AIndex: Integer): Integer;
begin
  Result := GetAddrList(AIndex).GetAddrCount();
end;

function TCellValueList.GetAddrStr
  (AIndex: Integer;
  AFrom: Integer;
  ATo: Integer): string;
begin
  Result := GetAddrList(AIndex).GetAddrStr(AFrom, ATo);
end;

function TCellAddrList.IsInterSection
  (ABegin1: Integer;
  AEnd1: Integer;
  ABegin2: Integer;
  AEnd2: Integer;
  var ABeginOut: Integer;
  out AEndOut: Integer): Boolean;
begin
  Result := False;
  ABeginOut := -999;
  AEndOut := -999;

  {确保Begin<End}
  if ABegin1 > AEnd1 then SwapInt(ABegin1, AEnd1);
  if ABegin2 > AEnd2 then SwapInt(ABegin2, AEnd2);
  {确保Begin1这一段在Begin2这一段前}
  if ABegin1 > ABegin2 then
  begin
    SwapInt(ABegin1, ABegin2);
    SwapInt(AEnd1, AEnd2);
  end;

  {若ABegin2小于等于AEnd1，则一定相交，且交集以ABegin2开始}
  if ABegin2 <= AEnd1 then
  begin
    Result := True;
    ABeginOut := ABegin2;
    if AEnd1 < AEnd2 then
      AEndOut := AEnd1
    else
      AEndOut := AEnd2
  end;

  Assert(ABeginOut <= AEndOut, '不明原因,InterSection函数错误');
end;

function TCellAddrList.AddrToStr(PCellAddr: PtagCellAddr): string;
var
  Multiplier: Integer;
  Remainder: Integer;
begin
  Result := '';
  Assert((PCellAddr^.RowT > 0) and (PCellAddr^.RowB > 0) and
    (PCellAddr^.ColL > 0) and (PCellAddr^.ColR > 0),
      '不明错误，AddrToStr函数中PCellAddr的值出错');

  Multiplier := (PCellAddr^.ColL - 1) div 26;
  Remainder := (PCellAddr^.ColL - 1) mod 26;

  if Multiplier > 0 then Result := Result + chr(Multiplier + 65);
  Result := Result + chr(Remainder + 65) + IntToStr(PCellAddr^.RowT);

  if (PCellAddr^.RowT <> PCellAddr^.RowB) or
    (PCellAddr^.ColL <> PCellAddr^.ColR) then
  begin
    Result := Result + ':';
    Multiplier := (PCellAddr^.ColR - 1) div 26;
    Remainder := (PCellAddr^.ColR - 1) mod 26;

    if Multiplier > 0 then Result := Result + chr(Multiplier + 65);
    Result := Result + chr(Remainder + 65) + IntToStr(PCellAddr^.RowB);
  end;
end;

function TCellValueList.GetAddrList(AIndex: Integer): TCellAddrList;
begin
  Result := TCellAddrList(FValueList.Items[AIndex]);
end;

function TCellValueList.GetXlsRange
  (AIndex: Integer;
  ASheet: TExcelWorksheet): {$WARNINGS OFF}ExcelRange;
{$WARNINGS ON}
begin
  Result := GetAddrList(AIndex).GetXlsRange(ASheet);
end;

function TCellAddrList.GetXlsRange(ASheet: TExcelWorksheet): {$WARNINGS OFF}ExcelRange;
{$WARNINGS ON}
var
  i: Integer;
  Str: string;
  AddrFrom, AddrTo: Integer;
begin
  Assert(FAddrList.Count > 0, '调用错误，地址链中无单元');

  AddrFrom := 0;
  AddrTo := AddrsEachRange - 1;
  Str := GetAddrStr(AddrFrom, AddrTo);
  Result := ASheet.Range[Str, EmptyParam];

  for i := 1 to (FAddrList.Count div AddrsEachRange) do
  begin
    AddrFrom := i * AddrsEachRange;
    AddrTo := (i + 1) * AddrsEachRange - 1;
    if AddrTo > FAddrList.Count - 1 then
      AddrTo := FAddrList.Count - 1;
    Str := GetAddrStr(AddrFrom, AddrTo);
    Result := ASheet.Application.Union(Result, ASheet.Range[Str, EmptyParam],
      EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, 0);
  end;

end;

procedure TCell.SetFormula(AValue: Variant);
var
  Row, Col: Integer;
  CellAddr: tagCellAddr;
begin
  {扫描要设置的所有单元格，若某个单元格是处于合并单元格中
   则对该单元格的设置将改成对合并单元格的左上角单元格进行
   设置，否则若合并单元格中除左上角的单元格以外有其余的单
   元格有值，会导致调用EXCEL的合并功能时失败}
  for Row := FRowTOfCur to FRowBOfCur do
    for Col := FColLOfCur to FColROfCur do
    begin
      if FMerges.IsMerged(Row, Col, CellAddr) then
        FFormula.SetCurAddr(CellAddr.RowT, CellAddr.ColL)
      else
        FFormula.SetCurAddr(Row, Col);
      FFormula.Formula := AValue;
    end;
end;

destructor TCell.Destroy;
begin
  FFormula.Destroy;
  FFonts.Destroy;
  FBorders.Destroy;
  FMerges.Destroy;
  FNumberFormatLocal.Destroy;
  FHorizontalAlignment.Destroy;
  FVerticalalignment.Destroy;
  FInterior.Destroy;
  FWrapText.Destroy;
end;

constructor TCell.Create;
begin
  FFormula := TCellFormula.Create;
  FFonts := TCellFont.Create;
  FBorders := TCellBorder.Create;
  FMerges := TCellMerge.Create;
  FNumberFormatLocal := TStringValueList.Create;
  FHorizontalAlignment := TLongWordValueList.Create;
  FVerticalAlignment := TLongWordValueList.Create;
  FInterior := TInterior.Create;
  FWrapText := TCellWrapText.Create;
end;

procedure TCell.SetMerge(AValue: Boolean);
begin
  {把合并的单元格，除左上角的单元格钋，其余Formula的都清空，
   因为如果除左上角单元以外，其余的单元格有值，将会导致调用Excel
   的合并功能时失败}
  if FRowBOfCur > FRowTOfCur then
  begin
    FFormula.SetCurAddr(FRowTOfCur + 1, FColLOfCur, FRowBOfCur, FColROfCur);
    FFormula.Formula := '';
  end;
  if FColLOfCur < FColROfCur then
  begin
    FFormula.SetCurAddr(FRowTOfCur, FColLOfCur + 1, FRowTOfCur, FColROfCur);
    FFormula.Formula := '';
  end;

  {设置Merge属性}
  FMerges.SetCurAddr(FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
  FMerges.Merge := AValue;
end;

procedure TCell.SetNumberFormatLocal(AValue: string);
begin
  FNumberFormatLocal.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

function TExcelSheet.GetHPageBreaks: THPageBreak;
begin
  Result := FHPageBreaks;
end;

procedure THPageBreak.Print(ASheet: TExcelWorksheet);
var
  i: Integer;
  ce: OleVariant;
begin
  {输出HPageBreak}

  {若没有任何单元设置分页符，则退出}
  if FHPageBreak.GetCount() = 0 then Exit;

  Assert((FHPageBreak.GetCount() <= 1) and
    (FHPageBreak.Value[0] = 1),
    '设值错误，在FHPageBreak的值链中要么没有，要么只有1值，即设为有水平分页符的单元地址链');

  for i := 0 to FHPageBreak.GetAddrCount(0) - 1 do
  begin
    {一次只能输出一个分页符,故只能使用GetAddrStr(0,i,i)取一个
     单元格的地址进行处理}
    ce := ASheet.Range[FHPageBreak.GetAddrStr(0, i, i), EMPTYPARAM];
    ASheet.HPageBreaks.Add(ce);
  end;

end;

procedure THPageBreak.AddBefore(ARowBefore: Integer);
begin
  FHPageBreak.SetVA(1, ARowBefore, 1);
end;

destructor THPageBreak.Destroy;
begin
  FHPageBreak.Destroy;
end;

constructor THPageBreak.Create;
begin
  FHPageBreak := TIntegerValueList.Create;
end;

procedure TCell.SetHorizontalAlignment(AValue: LongWord);
begin
  FHorizontalAlignment.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCell.SetVerticalAlignment(AValue: LongWord);
begin
  FVerticalAlignment.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

function TCell.GetInterior: TInterior;
begin
  FInterior.SetCurAddr(FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
  Result := FInterior;
end;

function TLongWordValueList.SetVA
  (AValue: LongWord;
  ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer): Integer;
var
  NewValue: PLongWord;
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);
  New(NewValue);
  NewValue^ := AValue;
  Result := SetVA(Pointer(NewValue), ARowT, AColL, ARowB, AColR);
end;

function TLongWordValueList.IsEqual
  (A: Pointer;
  B: Pointer): Boolean;
begin
  if PLongWord(A)^ = PLongWord(B)^ then
    Result := True
  else
    Result := False;
end;

procedure TLongWordValueList.FreePointer(AP: Pointer);
begin
  Dispose(PLongWord(AP));
end;

function TLongWordValueList.GetValue(AIndex: Integer): LongWord;
begin
  Result := PLongWord(GetAddrList(AIndex).Value)^;
end;

procedure TCellMerge.SetMerge(AValue: Boolean);
begin
  FMerge.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCellMerge.Print(ASheet: TExcelWorksheet);
var
  i, j: Integer;
begin
  {输出Merge}
  with FMerge do
    for i := 0 to GetCount - 1 do
      if GetAddrCount(i) > 0 then
        for j := 0 to (GetAddrCount(i) div AddrsEachRange) do
          ASheet.Range[GetAddrStr(i, j * AddrsEachRange, (j + 1) * AddrsEachRange - 1),
            EmptyParam].MergeCells := Value[i];

end;

procedure TCellMerge.SetCurAddr
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer);
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  FRowTOfCur := ARowT;
  FColLOfCur := AColL;
  FRowBOfCur := ARowB;
  FColROfCur := AColR;

end;

destructor TCellMerge.Destroy;
begin
  FMerge.Destroy;
end;

constructor TCellMerge.Create;
begin
  FMerge := TBooleanValueList.Create;
end;

function TCellMerge.IsMerged
  (ARow: Integer;
  ACol: Integer;
  var ACellLT: tagCellAddr): Boolean;
var
  PMergeValue: PBoolean;
  Index: Integer;
begin
  Result := False;

  if FMerge.GetCount() = 0 then Exit;

  Assert(FMerge.GetCount() <= 2, '错误，FMerge只有两种值0,1');

  {Merge为True的是合并的单元}
  New(PMergeValue);
  PMergeValue^ := True;
  Index := FMerge.IndexFromValue(Pointer(PMergeValue));
  if Index >= 0 then
    Result := FMerge.IsInAddrList(Index, ARow, ACol, ACellLT);
  Dispose(PMergeValue);
end;

function TCellAddrList.IsInAddrList
  (ARow: Integer;
  ACol: Integer;
  var ACellLT: tagCellAddr): Boolean;
var
  i: Integer;
  PAddr: PtagCellAddr;
begin
  Result := False;

  for i := 0 to GetAddrCount() - 1 do
  begin
    PAddr := PtagCellAddr(FAddrList.Items[i]);
    if (ARow >= PAddr^.RowT) and (ARow <= PAddr^.RowB) and
      (ACol >= PAddr^.ColL) and (ACol <= PAddr^.ColR) then
    begin
      ACellLT.RowT := PAddr^.RowT;
      ACellLT.RowB := PAddr^.RowT;
      ACellLT.ColL := PAddr^.ColL;
      ACellLT.ColR := PAddr^.ColR;
      Result := True;
      Exit;
    end;
  end;
end;

function TCellValueList.IsInAddrList
  (AIndex: Integer;
  ARow: Integer;
  ACol: Integer;
  var ACellLT: tagCellAddr): Boolean;
begin
  Result := GetAddrList(AIndex).IsInAddrList(ARow, ACol, ACellLT);
end;

function TCellValueList.IndexFromValue(AValue: Pointer): Integer;
var
  i: Integer;
begin
  Result := -999;
  for i := 0 to GetCount() - 1 do
    if IsEqual(FValueList.Items[i], AValue) then Result := i;
end;

procedure TCellBorder.SetCurEdge(AEdge: SmallInt);
begin
  FEdgeOfCur := XlsEdgeToIndex(AEdge);
end;

procedure TCellBorder.SetLineStyle(AValue: Integer);
begin
  Assert((FColROfCur >= FColLOfCur) and (FRowBOfCur >= FRowTOfCur), '选择矩阵错误');

  //如果是选择矩阵内的垂直边线，那么当只有一列的时候不能对xlInsideVertical边设置
  if (IndexToXlsEdge(FEdgeOfCur) = xlInsideVertical) and (FColROfCur = FColLOfCur) then
    Exit;
  //如果是选择矩阵内的水平边线，那么当只有一行的时候不能对xlInsideHorizontal边设置
  if (IndexToXlsEdge(FEdgeOfCur) = xlInsideHorizontal) and (FRowBOfCur = FRowTOfCur)
    then Exit;

  FEdgesLineStyle[FEdgeOfCur].SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur,
    FColROfCur);
end;

function TCellBorder.XlsEdgeToIndex(AXlsEdge: SmallInt): SmallInt;
begin
  Result := -999;
  case AXlsEdge of
    xlEdgeLeft: Result := 0;
    xlEdgeTop: Result := 1;
    xlEdgeBottom: Result := 2;
    xlEdgeRight: Result := 3;
    xlInsideVertical: Result := 4;
    xlInsideHorizontal: Result := 5;
  else
    Assert(False, 'AXlsEdge边值范围错误');
  end;
end;

function TCellBorder.IndexToXlsEdge(AIndex: SmallInt): SmallInt;
begin
  Result := -999;
  case AIndex of
    0: Result := xlEdgeLeft;
    1: Result := xlEdgeTop;
    2: Result := xlEdgeBottom;
    3: Result := xlEdgeRight;
    4: Result := xlInsideVertical;
    5: Result := xlInsideHorizontal;
  else
    Assert(False, '索引超出了范围');
  end;
end;

procedure TInterior.SetCurAddr
  (ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer);
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  FRowTOfCur := ARowT;
  FColLOfCur := AColL;
  FRowBOfCur := ARowB;
  FColROfCur := AColR;

end;

procedure TInterior.SetColorIndex(AColorIndex: Integer);
begin
  FColorIndex.SetVA(AColorIndex, FRowTOfCur, FColLOfcur, FRowBOfCur, FColROfCur);
end;

destructor TInterior.Destroy;
begin
  FColorIndex.Destroy;
end;

constructor TInterior.Create;
begin
  FColorIndex := TIntegerValueList.Create;
end;

procedure TInterior.Print(ASheet: TExcelWorksheet);
var
  i: Integer;
begin
  {输出ColorIndex}
  for i := 0 to FColorIndex.GetCount - 1 do
    if FColorIndex.GetAddrCount(i) > 0 then
      FColorIndex.GetXlsRange(i, ASheet).Interior.ColorIndex := FColorIndex.Value[i];
end;

function TBooleanValueList.SetVA
  (AValue: Boolean;
  ARowT: Integer;
  AColL: Integer;
  ARowB: Integer;
  AColR: Integer): Integer;
var
  NewValue: PBoolean;
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);
  New(NewValue);
  NewValue^ := AValue;
  Result := SetVA(Pointer(NewValue), ARowT, AColL, ARowB, AColR);
end;

procedure TBooleanValueList.FreePointer(AP: Pointer);
begin
  Dispose(PBoolean(Ap));
end;

function TBooleanValueList.IsEqual
  (A: Pointer;
  B: Pointer): Boolean;
begin
  if not (PBoolean(A)^ xor PBoolean(B)^) then
    Result := True
  else
    Result := False;
end;

function TBooleanValueList.GetValue(AIndex: Integer): Boolean;
begin
  Result := PBoolean(GetAddrList(AIndex).Value)^;
end;

procedure TCell.FullLineBTLRVH(ALineStyle: Integer);
begin
  Borders[xlEdgeTop].LineStyle := ALineStyle;
  Borders[xlEdgeBottom].LineStyle := ALineStyle;
  Borders[xlEdgeLeft].LineStyle := ALineStyle;
  Borders[xlEdgeRight].LineStyle := ALineStyle;
  Borders[xlInsideVertical].LineStyle := ALineStyle;
  Borders[xlInsideHorizontal].LineStyle := ALineStyle;
end;

{ TCellWrapText }

constructor TCellWrapText.Create;
begin
  FWrapText := TBooleanValueList.Create;
end;

destructor TCellWrapText.Destroy;
begin
  FWrapText.Destroy;
end;

procedure TCellWrapText.Print(ASheet: TExcelWorksheet);
var
  j, k: Integer;
begin
  {输出WrapText}
  with FWrapText do
    for j := 0 to GetCount - 1 do
      if GetAddrCount(j) > 0 then
        for k := 0 to (GetAddrCount(j) div AddrsEachRange) do
          ASheet.Range[GetAddrStr(j, k * AddrsEachRange, (k + 1) * AddrsEachRange - 1),
            EmptyParam].WrapText := Value[j];
end;

procedure TCellWrapText.SetCurAddr(ARowT, AColL, ARowB, AColR: Integer);
begin
  VerifyAndFixAddrParam(ARowT, AColL, ARowB, AColR);

  FRowTOfCur := ARowT;
  FColLOfCur := AColL;
  FRowBOfCur := ARowB;
  FColROfCur := AColR;

end;

procedure TCellWrapText.SetWrapText(AValue: Boolean);
begin
  FWrapText.SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
end;

procedure TCell.SetWrapText(AValue: Boolean);
begin
  {设置WrapText属性}
  FWrapText.SetCurAddr(FRowTOfCur, FColLOfCur, FRowBOfCur, FColROfCur);
  FWrapText.WrapText := AValue;
end;

procedure TCellBorder.SetWeight(AValue: Integer);
begin
  Assert((FColROfCur >= FColLOfCur) and (FRowBOfCur >= FRowTOfCur), '选择矩阵错误');

  //如果是选择矩阵内的垂直边线，那么当只有一列的时候不能对xlInsideVertical边设置
  if (IndexToXlsEdge(FEdgeOfCur) = xlInsideVertical) and (FColROfCur = FColLOfCur) then
    Exit;
  //如果是选择矩阵内的水平边线，那么当只有一行的时候不能对xlInsideHorizontal边设置
  if (IndexToXlsEdge(FEdgeOfCur) = xlInsideHorizontal) and (FRowBOfCur = FRowTOfCur)
    then Exit;

  FEdgesWeight[FEdgeOfCur].SetVA(AValue, FRowTOfCur, FColLOfCur, FRowBOfCur,
    FColROfCur);
end;

end.
