{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

unit CnColorGrid;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：颜色选择框类库
* 单元作者：图墓
* 备    注：由 Childe Ng 移植
* 开发平台：PWinXP SP2 + Delphi 5
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnColorGrid.pas
* 修改记录：2008.07.13 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, Graphics, Messages, Dialogs, SysUtils,
  Controls, StdCtrls, ComCtrls, ExtCtrls, Grids, ImgList;

type
  TColorSetInfoItem = record
    defaultRowCount: Integer;
    defaultColCount: Integer;
  end;

  TColorSet = (csSafeColors, cs16Colors, csGrayColors, csRedGreenColors,
    csHSBColors, csCustomColors);

  TCustomColorType = (ccDec, ccHex, ccHtml);

  TAutoSizeBy = (asRowCol, asSameWidth, asSameHeight);

  TOnSelectedColor = procedure(Sender: TObject; const Color: TColor) of object;

type
  TCnColorGrid = class(TDrawGrid)
  private
    FAutoSize: Boolean;
    FAutoSizeBy: TAutoSizeBy;
    FHotTrace: Boolean;
    FGridLineColor1: TColor;
    FGridLineColor2: TColor;
    FSelectLineColor: TColor;
    FSelectRangeColor: TColor;
    FCustomColors: Tstrings;
    FCustomRowCount: Integer;
    FCustomColCount: Integer;
    FCustomColorType: TCustomColorType;
    FColorSet: TColorSet;
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetAutoSizeBy(Value: TAutoSizeBy);
    procedure SetCustomColors(Value: TStrings);
    procedure SetColorSet(Value: TColorSet);
    procedure SetCustomRowCount(Value: Integer);
    procedure SetCustomColCount(Value: Integer);
    procedure ChangeRowCol;
    procedure Resize; override;
    function GetColorFromColorset(const Index: Integer): TColor;
    function GetSelectedColor: TColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawGridColor;
    function GetGridHeight: Integer;
    function GetGridWidth: Integer;

    function PreColorSet: Boolean;
    function NextColorSet: Boolean;

    property SelectedColor: TColor read GetSelectedColor;
  published
    property AutoSize: Boolean read FAutoSize write FAutoSize default True;
    property AutoSizeBy: TAutoSizeBy read FAutoSizeBy write SetAutoSizeBy default asRowCol;
    property HotTrace: Boolean read FHotTrace write FHotTrace default True;
    property GridLineColor1: TColor read FGridLineColor1 write FGridLineColor1 default clGrayText;
    property GridLineColor2: TColor read FGridLineColor2 write FGridLineColor2 default clWindow;
    property SelectLineColor: TColor read FSelectLineColor write FSelectLineColor default clBlack;
    property SelectRangeColor: TColor read FSelectRangeColor write FSelectRangeColor default clWindow;
    property ColorSet: TColorSet read FColorSet write SetColorSet default csSafeColors;
    property CustomColors: Tstrings read FCustomColors write SetCusTomColors;
    property CustomRowCount: Integer read FCustomRowCount write SetCustomRowCount;
    property CustomColCount: Integer read FCustomColCount write SetCustomColCount;
    property CustomColorType: TCustomColorType read FCustomColorType write FCustomColorType default ccHex;
  end;

  TCnColorGridPanel = class(TCustomPanel)
  private
    FColorGrid:TCnColorGrid;
    FPanelTool: TPanel;
    FToolBar: TToolBar;
    FLabeTile: TLabel;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FPanelToolOnClick: TNotifyEvent;
    FColorDialog:TColorDialog;
    FOnSelectedColor:TOnSelectedColor;
    FAutoLableColor:Boolean;
    procedure ImageListChange(Sender: TObject);
  protected
    procedure DoReSize;
    procedure ColorGridClick(Sender: TObject);
    procedure PanelToolClick(Sender: TObject);
    procedure PanelReSize(Sender: TObject);
    procedure BtnPreClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnMoreClick(Sender: TObject);
    function GetColorGridBackGround:TColor;
    function GetColorGridClew:AnsiString;
    function GetToolPanelVisible:Boolean;
    function GetAutoSizeBy:TAutoSizeBy;
    procedure SetColorGridBackGround(Value:TColor);
    procedure SetAutoSizeBy(Value:TAutoSizeBy);
    procedure SetToolPanelVisible(Value:Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure ColorGridMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd;override;
    procedure DoInit;
    function GetColor:TColor;
    function GetHtmlColor(C: TColor): AnsiString;
  published
    property AutoLableColor:Boolean read FAutoLableColor write FAutoLableColor default True;
    property OnSelectedColor:TOnSelectedColor read FOnSelectedColor write FOnSelectedColor;
    property ColorDialog:TColorDialog  read FColorDialog write FColorDialog;
    property PanelToolOnClick: TNotifyEvent read FPanelToolOnClick write FPanelToolOnClick;
    property ColorGridBackGround: TColor read GetColorGridBackGround write SetColorGridBackGround;
    property ColorGrid:TCnColorGrid  read FColorGrid write FColorGrid;
    property AutoSizeBy: TAutoSizeBy read GetAutoSizeBy write SetAutoSizeBy;
    property Images: TCustomImageList read FImages write SetImages;
    property ToolPanelVisible:Boolean read GetToolPanelVisible write SetToolPanelVisible;
    property OnResize;
  end;

implementation

uses
  CnGraphUtils;

const
  SSafeColors: array[0..215] of Integer = (
    $000000, $330000, $660000, $990000, $CC0000, $FF0000,
    $003300, $333300, $663300, $993300, $CC3300, $FF3300,
    $000033, $330033, $660033, $990033, $CC0033, $FF0033,
    $003333, $333333, $663333, $993333, $CC3333, $FF3333,
    $000066, $330066, $660066, $990066, $CC0066, $FF0066,
    $003366, $333366, $663366, $993366, $CC3366, $FF3366,
    $000099, $330099, $660099, $990099, $CC0099, $FF0099,
    $003399, $333399, $663399, $993399, $CC3399, $FF3399,
    $0000CC, $3300CC, $6600CC, $9900CC, $CC00CC, $FF00CC,
    $0033CC, $3333CC, $6633CC, $9933CC, $CC33CC, $FF33CC,
    $0000FF, $3300FF, $6600FF, $9900FF, $CC00FF, $FF00FF,
    $0033FF, $3333FF, $6633FF, $9933FF, $CC33FF, $FF33FF,
    $006600, $336600, $666600, $996600, $CC6600, $FF6600,
    $009900, $339900, $669900, $999900, $CC9900, $FF9900,
    $006633, $336633, $666633, $996633, $CC6633, $FF6633,
    $009933, $339933, $669933, $999933, $CC9933, $FF9933,
    $006666, $336666, $666666, $996666, $CC6666, $FF6666,
    $009966, $339966, $669966, $999966, $CC9966, $FF9966,
    $006699, $336699, $666699, $996699, $CC6699, $FF6699,
    $009999, $339999, $669999, $999999, $CC9999, $FF9999,
    $0066CC, $3366CC, $6666CC, $9966CC, $CC66CC, $FF66CC,
    $0099CC, $3399CC, $6699CC, $9999CC, $CC99CC, $FF99CC,
    $0066FF, $3366FF, $6666FF, $9966FF, $CC66FF, $FF66FF,
    $0099FF, $3399FF, $6699FF, $9999FF, $CC99FF, $FF99FF,
    $00CC00, $33CC00, $66CC00, $99CC00, $CCCC00, $FFCC00,
    $00FF00, $33FF00, $66FF00, $99FF00, $CCFF00, $FFFF00,
    $00CC33, $33CC33, $66CC33, $99CC33, $CCCC33, $FFCC33,
    $00FF33, $33FF33, $66FF33, $99FF33, $CCFF33, $FFFF33,
    $00CC66, $33CC66, $66CC66, $99CC66, $CCCC66, $FFCC66,
    $00FF66, $33FF66, $66FF66, $99FF66, $CCFF66, $FFFF66,
    $00CC99, $33CC99, $66CC99, $99CC99, $CCCC99, $FFCC99,
    $00FF99, $33FF99, $66FF99, $99FF99, $CCFF99, $FFFF99,
    $00CCCC, $33CCCC, $66CCCC, $99CCCC, $CCCCCC, $FFCCCC,
    $00FFCC, $33FFCC, $66FFCC, $99FFCC, $CCFFCC, $FFFFCC,
    $00CCFF, $33CCFF, $66CCFF, $99CCFF, $CCCCFF, $FFCCFF,
    $00FFFF, $33FFFF, $66FFFF, $99FFFF, $CCFFFF, $FFFFFF
    );

  S16Colors: array[0..15] of Integer = (
    $000000, $000080, $008000, $008080, $800000, $800080, $808000, $C0C0C0,
    $808080, $0000FF, $00FF00, $00FFFF, $FF0000, $FF00FF, $FFFF00, $FFFFFF
    );

  SGrayColors: array[0..255] of Integer = (//256灰度色
    $000000, $010101, $020202, $030303, $040404, $050505, $060606, $070707,
    $080808, $090909, $0A0A0A, $0B0B0B, $0C0C0C, $0D0D0D, $0E0E0E, $0F0F0F,
    $101010, $111111, $121212, $131313, $141414, $151515, $161616, $171717,
    $181818, $191919, $1A1A1A, $1B1B1B, $1C1C1C, $1D1D1D, $1E1E1E, $1F1F1F,
    $202020, $212121, $222222, $232323, $242424, $252525, $262626, $272727,
    $282828, $292929, $2A2A2A, $2B2B2B, $2C2C2C, $2D2D2D, $2E2E2E, $2F2F2F,
    $303030, $313131, $323232, $333333, $343434, $353535, $363636, $373737,
    $383838, $393939, $3A3A3A, $3B3B3B, $3C3C3C, $3D3D3D, $3E3E3E, $3F3F3F,
    $404040, $414141, $424242, $434343, $444444, $454545, $464646, $474747,
    $484848, $494949, $4A4A4A, $4B4B4B, $4C4C4C, $4D4D4D, $4E4E4E, $4F4F4F,
    $505050, $515151, $525252, $535353, $545454, $555555, $565656, $575757,
    $585858, $595959, $5A5A5A, $5B5B5B, $5C5C5C, $5D5D5D, $5E5E5E, $5F5F5F,
    $606060, $616161, $626262, $636363, $646464, $656565, $666666, $676767,
    $686868, $696969, $6A6A6A, $6B6B6B, $6C6C6C, $6D6D6D, $6E6E6E, $6F6F6F,
    $707070, $717171, $727272, $737373, $747474, $757575, $767676, $777777,
    $787878, $797979, $7A7A7A, $7B7B7B, $7C7C7C, $7D7D7D, $7E7E7E, $7F7F7F,
    $808080, $818181, $828282, $838383, $848484, $858585, $868686, $878787,
    $888888, $898989, $8A8A8A, $8B8B8B, $8C8C8C, $8D8D8D, $8E8E8E, $8F8F8F,
    $909090, $919191, $929292, $939393, $949494, $959595, $969696, $979797,
    $989898, $999999, $9A9A9A, $9B9B9B, $9C9C9C, $9D9D9D, $9E9E9E, $9F9F9F,
    $A0A0A0, $A1A1A1, $A2A2A2, $A3A3A3, $A4A4A4, $A5A5A5, $A6A6A6, $A7A7A7,
    $A8A8A8, $A9A9A9, $AAAAAA, $ABABAB, $ACACAC, $ADADAD, $AEAEAE, $AFAFAF,
    $B0B0B0, $B1B1B1, $B2B2B2, $B3B3B3, $B4B4B4, $B5B5B5, $B6B6B6, $B7B7B7,
    $B8B8B8, $B9B9B9, $BABABA, $BBBBBB, $BCBCBC, $BDBDBD, $BEBEBE, $BFBFBF,
    $C0C0C0, $C1C1C1, $C2C2C2, $C3C3C3, $C4C4C4, $C5C5C5, $C6C6C6, $C7C7C7,
    $C8C8C8, $C9C9C9, $CACACA, $CBCBCB, $CCCCCC, $CDCDCD, $CECECE, $CFCFCF,
    $D0D0D0, $D1D1D1, $D2D2D2, $D3D3D3, $D4D4D4, $D5D5D5, $D6D6D6, $D7D7D7,
    $D8D8D8, $D9D9D9, $DADADA, $DBDBDB, $DCDCDC, $DDDDDD, $DEDEDE, $DFDFDF,
    $E0E0E0, $E1E1E1, $E2E2E2, $E3E3E3, $E4E4E4, $E5E5E5, $E6E6E6, $E7E7E7,
    $E8E8E8, $E9E9E9, $EAEAEA, $EBEBEB, $ECECEC, $EDEDED, $EEEEEE, $EFEFEF,
    $F0F0F0, $F1F1F1, $F2F2F2, $F3F3F3, $F4F4F4, $F5F5F5, $F6F6F6, $F7F7F7,
    $F8F8F8, $F9F9F9, $FAFAFA, $FBFBFB, $FCFCFC, $FDFDFD, $FEFEFE, $FFFFFF
    );

  SRedGreenColors: array[0..239] of Integer = (
    $0000FF, $0004FF, $0009FF, $000DFF, $0011FF, $0015FF, $001AFF, $001EFF,
    $0022FF, $0026FF, $002BFF, $002FFF, $0033FF, $0037FF, $003CFF,
    $0040FF, $0044FF, $0048FF, $004DFF, $0051FF, $0055FF, $0059FF, $005EFF,
    $0062FF, $0066FF, $006AFF, $006FFF, $0073FF, $0077FF, $007BFF,
    $0080FF, $0084FF, $0088FF, $008CFF, $0090FF, $0095FF, $0099FF, $009DFF,
    $00A1FF, $00A6FF, $00AAFF, $00AEFF, $00B2FF, $00B7FF, $00BBFF,
    $00BFFF, $00C4FF, $00C8FF, $00CCFF, $00D0FF, $00D5FF, $00D9FF, $00DDFF,
    $00E1FF, $00E6FF, $00EAFF, $00EEFF, $00F2FF, $00F7FF, $00FBFF,
    $00FFFF, $00FFFB, $00FFF7, $00FFF2, $00FFEE, $00FFEA, $00FFE5, $00FFE1,
    $00FFDD, $00FFD9, $00FFD4, $00FFD0, $00FFCC, $00FFC8, $00FFC3,
    $00FFBF, $00FFBB, $00FFB7, $00FFB3, $00FFAE, $00FFAA, $00FFA6, $00FFA1,
    $00FF9D, $00FF99, $00FF95, $00FF90, $00FF8C, $00FF88, $00FF84,
    $00FF80, $00FF7B, $00FF77, $00FF73, $00FF6F, $00FF6A, $00FF66, $00FF62,
    $00FF5E, $00FF59, $00FF55, $00FF51, $00FF4C, $00FF48, $00FF44,
    $00FF40, $00FF3C, $00FF37, $00FF33, $00FF2F, $00FF2B, $00FF26, $00FF22,
    $00FF1E, $00FF1A, $00FF15, $00FF11, $00FF0D, $00FF08, $00FF04,
    $00FF00, $04FF00, $08FF00, $0DFF00, $11FF00, $15FF00, $19FF00, $1EFF00,
    $22FF00, $26FF00, $2AFF00, $2FFF00, $33FF00, $37FF00, $3CFF00,
    $40FF00, $44FF00, $48FF00, $4CFF00, $51FF00, $55FF00, $59FF00, $5DFF00,
    $62FF00, $66FF00, $6AFF00, $6FFF00, $73FF00, $77FF00, $7BFF00,
    $80FF00, $84FF00, $88FF00, $8CFF00, $90FF00, $95FF00, $99FF00, $9DFF00,
    $A1FF00, $A6FF00, $AAFF00, $AEFF00, $B2FF00, $B7FF00, $BBFF00,
    $BFFF00, $C3FF00, $C8FF00, $CCFF00, $D0FF00, $D4FF00, $D9FF00, $DDFF00,
    $E1FF00, $E5FF00, $EAFF00, $EEFF00, $F2FF00, $F7FF00, $FBFF00,
    $FFFF00, $FFFB00, $FFF700, $FFF200, $FFEE00, $FFEA00, $FFE600, $FFE100,
    $FFDD00, $FFD900, $FFD400, $FFD000, $FFCC00, $FFC800, $FFC400,
    $FFBF00, $FFBB00, $FFB700, $FFB200, $FFAE00, $FFAA00, $FFA600, $FFA200,
    $FF9D00, $FF9900, $FF9500, $FF9000, $FF8C00, $FF8800, $FF8400,
    $FF8000, $FF7B00, $FF7700, $FF7300, $FF6F00, $FF6A00, $FF6600, $FF6200,
    $FF5E00, $FF5900, $FF5500, $FF5100, $FF4C00, $FF4800, $FF4400,
    $FF4000, $FF3C00, $FF3700, $FF3300, $FF2F00, $FF2A00, $FF2600, $FF2200,
    $FF1E00, $FF1A00, $FF1500, $FF1100, $FF0D00, $FF0800, $FF0400
    );

  ColorsetInfo: array[TColorSet] of TColorSetInfoItem = (
    (defaultRowCount: 18; defaultColCount: 12),
    (defaultRowCount: 2; defaultColCount: 8),
    (defaultRowCount: 16; defaultColCount: 16),
    (defaultRowCount: 10; defaultColCount: 14),
    (defaultRowCount: 18; defaultColCount: 13),
    (defaultRowCount: 0; defaultColCount: 0)
    );

var
  SHSBColors: array[0..233] of Integer;

procedure InitHSBColors;
var
  Row, Col: Integer;
begin
 for Row := 1 to ColorsetInfo[csHSBColors].defaultRowCount do
   for Col := 1 to ColorsetInfo[csHSBColors].defaultColCount do
     SHSBColors[ColorsetInfo[csHSBColors].defaultColCount * (Row - 1) + Col - 1] :=
       HSLToRGB((Row - 1) / (ColorsetInfo[csHSBColors].defaultRowCount), 1, // 饱和度固定为 1
       (Col - 1) / (ColorsetInfo[csHSBColors].defaultColCount - 1));
end;

{ TCnColorGrid }

procedure TCnColorGrid.ChangeRowCol;
var
  ARow, ACol: Integer;
begin
  case ColorSet of
    csSafeColors..csHSBColors:
      begin
        ARow := ColorsetInfo[ColorSet].defaultRowCount;
        ACol := ColorsetInfo[ColorSet].defaultColCount;
      end;
    csCustomColors:
      begin
        ARow := CustomRowCount;
        ACol := customcolCount;
      end;
  else
    ARow := -1;
    ACol := -1;
  end;

  if ARow > -1 then
    RowCount := ARow;
  if ACol > -1 then
    ColCount := ACol;
  if AutoSize then
    ReSize;
end;

procedure TCnColorGrid.Resize;
begin
  case AutoSizeBy of
    asRowCol: begin
        Self.Width := GetGridWidth;
        Self.Height := GetGridHeight;
      end;
    asSameWidth:begin
       DefaultColWidth:=Round((Width - ColCount) / ColCount);
       Self.Height:= GetGridHeight;
    end;
    asSameHeight:begin
       Self.Width := GetGridWidth;
       DefaultRowHeight:=Round((Height - RowCount) / RowCount);
    end;
  end;
  inherited ReSize;
end;

constructor TCnColorGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomColors := TStringList.Create;
  ScrollBars := ssNone;
  Options := Options - [goVertLine];
  Options := Options - [goHorzLine];
  Options := Options - [goRangeSelect];
  FixedCols := 0;
  FixedRows := 0;
  DefaultColWidth := 15;
  DefaultRowHeight := 15;
  RowCount := 18;
  ColCount := 12;
  CustomRowCount := 10;
  CustomcolCount := 10;
  CustomColorType := ccHex;
  GridLineColor1 := clGrayText;
  GridLineColor2 := clWindow;
  SelectLineColor := clBlack;
  SelectRangeColor := clWindow;
  AutoSize := True;
  FAutoSizeBy:=asRowCol;
  HotTrace := True;
  ColorSet := csSafeColors;
  AutoSize:=True;
  if AutoSize then
    ReSize;
end;

destructor TCnColorGrid.Destroy;
begin
  FCustomColors.Free;
  inherited;
end;

procedure TCnColorGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  OldColor: TColor;
  OldWidth: integer;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := GetColorFromColorset(ARow * ColCount + ACol);
    FillRect(ARect);
  end;

  with Canvas do
  begin
    Brush.Style := bsClear;
    OldColor := Pen.Color;
    OldWidth := Pen.Width;
    Pen.Width := GridLineWidth;
    if gdSelected in AState then
    begin
      Pen.Color := FSelectLineColor;
      MoveTo(ARect.Left, ARect.Top);
      LineTo(ARect.Right, ARect.Top);
      MoveTo(ARect.Right, ARect.Top);
      LineTo(ARect.Right, ARect.Bottom);
      MoveTo(ARect.Right, ARect.Bottom);
      LineTo(ARect.Left, ARect.Bottom);
      MoveTo(ARect.Left, ARect.Bottom);
      LineTo(ARect.Left, ARect.Top);
      Pen.Color := FSelectRangeColor;
      MoveTo(ARect.Left + 1, ARect.Top + 1);
      LineTo(ARect.Right - 1, ARect.Top + 1);
      MoveTo(ARect.Right - 1, ARect.Top + 1);
      LineTo(ARect.Right - 1, ARect.Bottom - 1);
      MoveTo(ARect.Right - 1, ARect.Bottom - 1);
      LineTo(ARect.Left + 1, ARect.Bottom - 1);
      MoveTo(ARect.Left + 1, ARect.Bottom - 1);
      LineTo(ARect.Left + 1, ARect.Top + 1);
      Pen.Color := FSelectLineColor;
      MoveTo(ARect.Left + 2, ARect.Top + 2);
      LineTo(ARect.Right - 2, ARect.Top + 2);
      MoveTo(ARect.Right - 2, ARect.Top + 2);
      LineTo(ARect.Right - 2, ARect.Bottom - 2);
      MoveTo(ARect.Right - 2, ARect.Bottom - 2);
      LineTo(ARect.Left + 2, ARect.Bottom - 2);
      MoveTo(ARect.Left + 2, ARect.Bottom - 2);
      LineTo(ARect.Left + 2, ARect.Top + 2);
    end
    else
    begin
      Pen.Color := FGridLineColor1;
      MoveTo(ARect.Left, ARect.Top);
      LineTo(ARect.Right, ARect.Top);
      MoveTo(ARect.Left, ARect.Bottom - 1);
      LineTo(ARect.Left, ARect.Top);
      Pen.Color := FGridLineColor2;
      MoveTo(ARect.Right, ARect.Top + 1);
      LineTo(ARect.Right, ARect.Bottom);
      MoveTo(ARect.Right, ARect.Bottom);
      LineTo(ARect.Left, ARect.Bottom);
    end;
    Pen.Color := OldColor;
    Pen.Width := OldWidth;
  end;
  inherited;
end;

procedure TCnColorGrid.DrawGridColor;
var
  i, j, N: integer;
  Rl: TRect;
  SRect: TGridRect;
begin
  with SRect do
  begin
    Left := 0;
    Right := 0;
    Top := 0;
    Bottom := 0;
  end;
  Selection := SRect;
  for i := 0 to RowCount - 1 do
    for j := 0 to ColCount - 1 do
      with Canvas do
      begin
        Rl := CellRect(j, i);
        Brush.Style := bsSolid;
        N := I * ColCount + J;
        Brush.Color := GetColorFromColorset(N);
        FillRect(Rl);
        Pen.Color := FGridLineColor1;
        MoveTo(Rl.Left, Rl.Top);
        LineTo(Rl.Right, Rl.Top);
        MoveTo(Rl.Left, Rl.Bottom - 1);
        LineTo(Rl.Left, Rl.Top);
        Pen.Color := FGridLineColor2;
        MoveTo(Rl.Right, Rl.Top + 1);
        LineTo(Rl.Right, Rl.Bottom);
        MoveTo(Rl.Right, Rl.Bottom);
        LineTo(Rl.Left, Rl.Bottom);
      end;
end;

function TCnColorGrid.GetColorFromColorset(const Index: Integer): TColor;
var
  S: AnsiString;
begin
  case FColorSet of
    csSafeColors: Result := TColor(SSafeColors[Index]);
    cs16Colors: Result := TColor(S16Colors[Index]);
    csGrayColors: Result := TColor(SGrayColors[Index]);
    csRedGreenColors: Result := TColor(SRedGreenColors[Index]);
    csHSBColors: Result := TColor(SHSBColors[Index]);
    csCustomColors:
      begin
        Result := clWindow;
        if FCustomColors.Count = 0 then Exit;
        if Index >= FCustomColors.Count then Exit;
        S := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(FCustomColors[index]);
        case CustomColorType of
          ccDec: Result := TColor(StrToInt({$IFDEF DELPHI12_UP}String{$ENDIF}(S)));
          ccHex: Result := StringToColor('$' + {$IFDEF DELPHI12_UP}String{$ENDIF}(Copy(S, 6, 2) + Copy(S, 4, 2) + Copy(S, 2, 2)));
          ccHtml: Result := StringToColor('$' + {$IFDEF DELPHI12_UP}String{$ENDIF}(Copy(S, 6, 2) + Copy(S, 4, 2) + Copy(S, 2, 2)));
        end;
      end;
  else
    Result := clNone;
  end;
end;

function TCnColorGrid.GetGridHeight: Integer;
begin
  Result := (DefaultRowHeight + GridLineWidth) * RowCount + 3;
end;

function TCnColorGrid.GetGridWidth: Integer;
begin
  Result := (DefaultColWidth + GridLineWidth) * colCount + 3;
end;

function TCnColorGrid.GetSelectedColor: TColor;
begin
  Result := GetColorFromColorset(Row * ColCount + col);
end;

procedure TCnColorGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Arow, Acol: Integer;
  SRect: TGridRect;
begin
  MouseToCell(X, Y, Acol, Arow);
  if (ACol > -1) and (ARow > -1) then
  begin
    with SRect do
    begin
      Left := Acol;
      Right := Acol;
      Top := Arow;
      Bottom := Arow;
    end;
    if HotTrace then
      Selection := SRect;
  end;
  inherited MouseMove(Shift, X, Y);
end;

function TCnColorGrid.NextColorSet: Boolean;
begin
  if CustomColors.Count > 0 then
    Result := ColorSet < High(TColorSet)
  else
    Result := Ord(ColorSet) < (Ord(High(TColorSet)) - 1);
  if Result then
    ColorSet := TColorSet(Ord(ColorSet) + 1);
end;

function TCnColorGrid.PreColorSet: Boolean;
begin
  Result := ColorSet > Low(TColorSet);
  if Result then
    ColorSet := TColorSet(Ord(ColorSet) - 1);
end;

procedure TCnColorGrid.SetAutoSizeBy(Value: TAutoSizeBy);
begin
  if FAutoSizeBy <> Value then
  begin
    FAutoSizeBy := Value;
    ChangeRowCol;
    DrawGridColor;
  end;
end;

procedure TCnColorGrid.SetColorSet(Value: TColorSet);
begin
  if FColorSet <> Value then
  begin
    FColorSet := Value;
    ChangeRowCol;
    DrawGridColor;
  end;
end;

procedure TCnColorGrid.SetCustomColCount(Value: Integer);
begin
  if FcustomcolCount <> Value then
  begin
    FcustomcolCount := Value;
    if ColorSet = csCustomColors then
      ChangeRowCol;
  end;
end;

procedure TCnColorGrid.SetCustomRowCount(Value: Integer);
begin
  if FCustomRowCount <> Value then
  begin
    FCustomRowCount := Value;
    if ColorSet = csCustomColors then
      ChangeRowCol;
  end;
end;

procedure TCnColorGrid.SetCustomColors(Value: TStrings);
begin
  FCustomColors.Assign(Value);
end;


{ TCnColorGridPanel }

procedure TCnColorGridPanel.ColorGridClick(Sender: TObject);
begin
  if Assigned(FOnSelectedColor) then
     FOnSelectedColor(Self,ColorGrid.GetSelectedColor);
end;

procedure TCnColorGridPanel.ColorGridMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
begin
  FLabeTile.Caption:={$IFDEF DELPHI12_UP}String{$ENDIF}(GetColorGridClew)+' ';
  FLabeTile.Font.Color:=GetColor;
end;

constructor TCnColorGridPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TWinControl then
    Parent:=TWinControl(AOwner);
  Self.Caption:='';
  Self.BevelOuter:=bvNone;
  //PanelTool
  FPanelTool := TPanel.Create(Self);
  FPanelTool.Parent := Self;
  FPanelTool.Align := alTop;
  FPanelTool.BevelOuter := bvNone;
  FPanelTool.Height := 20;
  FPanelTool.OnClick:=PanelToolClick;
  FPanelTool.Caption:='';
  //ToolBar
  FToolBar := TToolBar.Create(Self);
  FToolBar.Parent := FPanelTool;
  FToolBar.ButtonHeight := 18;
  //FToolBar.DrawingStyle := ComCtrls.dsGradient;
  FToolBar.ParentColor := False;
  FToolBar.ShowHint := True;
  FToolBar.Align := alLeft;
  FToolBar.Height := 20;
  FToolBar.Width  :=80;
  with TToolButton.Create(self) do
  begin
    Parent := FToolBar;
    Caption := '';
    Hint := '打开选择对话框';
    OnClick := BtnMoreClick;
    ImageIndex := 2;
  end;
  with TToolButton.Create(FToolBar) do
  begin
    Parent := FToolBar;
    Style := tbsSeparator;
    Caption := '';
    Hint := '';
    Width := 8;
      //OnClick:=
  end;
  with TToolButton.Create(FToolBar) do
  begin
    Parent := FToolBar;
    Caption := '';
    Hint := '下一个颜色列表';
    OnClick := BtnNextClick;
    ImageIndex := 1;
  end;
  with TToolButton.Create(FToolBar) do
  begin
    Parent := FToolBar;
    Caption := '';
    Hint := '上一个颜色列表';
    OnClick := BtnPreClick;
    ImageIndex := 0;
  end;
  //LabeTile
  FLabeTile := TLabel.Create(self);
  FLabeTile.Parent := FPanelTool;
  FLabeTile.Caption := '';
  FLabeTile.Align := alRight;
  FLabeTile.Layout := tlCenter;
  FLabeTile.Font.Style := FLabeTile.Font.Style + [fsBold];
  FAutoLableColor:=True;
  //ColorGrid
  FColorGrid := TCnColorGrid.Create(self);
  FColorGrid.Parent := Self;
  FColorGrid.Align := alBottom;
  FColorGrid.OnResize:=PanelReSize;
  FColorGrid.OnMouseMove:=ColorGridMouseMove;
  FColorGrid.OnClick:=ColorGridClick;
  ColorGridBackGround:=clGray;
  //
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  //
  AutoSizeBy:=asRowCol;
  DoReSize;
end;

procedure TCnColorGridPanel.CreateWnd;
begin
  inherited CreateWnd;
  Parent := TWinControl(Owner);
  DoInit;
end;

destructor TCnColorGridPanel.Destroy;
begin
  FColorDialog:=nil;
  FImageChangeLink.Free;
  FColorGrid.Free;
  FLabeTile.Free;
  FToolBar.Free;
  FPanelTool.Free; 
  inherited Destroy;
end;

procedure TCnColorGridPanel.BtnMoreClick(Sender: TObject);
begin
  if Assigned(FColorDialog) then
  if FColorDialog.Execute then
  if Assigned(FOnSelectedColor) then
     FOnSelectedColor(Self,FColorDialog.Color);
end;

procedure TCnColorGridPanel.BtnNextClick(Sender: TObject);
begin
  if ColorGrid.NextColorSet then
  begin
    //Resize;
    //DrawGridColor;
  end;
end;

procedure TCnColorGridPanel.BtnPreClick(Sender: TObject);
begin
  if ColorGrid.PreColorSet then
  begin
    //Resize;
    //DrawGridColor;
  end;
end;

procedure TCnColorGridPanel.DoInit;
begin
  Self.Caption:='';
  Self.BevelOuter:=bvNone;
end;

procedure TCnColorGridPanel.DoReSize;
begin
  Self.Width := FColorGrid.GetGridWidth;
  if ToolPanelVisible then  
     Self.Height := FColorGrid.GetGridHeight + FPanelTool.Height
  else
     Self.Height := FColorGrid.GetGridHeight;
end;

function TCnColorGridPanel.GetAutoSizeBy: TAutoSizeBy;
begin
  Result:=FColorGrid.AutoSizeBy;
end;

function TCnColorGridPanel.GetColor: TColor;
begin
  Result:=FColorGrid.GetSelectedColor;
end;

function TCnColorGridPanel.GetColorGridBackGround: TColor;
begin
  Result:=FColorGrid.Color;
end;

function TCnColorGridPanel.GetColorGridClew: AnsiString;
begin
  Result:=GetHtmlColor(GetColor);
end;

function TCnColorGridPanel.GetHtmlColor(C: TColor): AnsiString;
begin
  result := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(format('#%2.2X%2.2X%2.2X',
    [C and $FF,
    (C shr 8) and $FF,
      (C shr 16) and $FF
      ]));
end;

function TCnColorGridPanel.GetToolPanelVisible: Boolean;
begin
  Result:=FPanelTool.Visible;
end;

procedure TCnColorGridPanel.ImageListChange(Sender: TObject);
begin
//
end;

procedure TCnColorGridPanel.PanelReSize(Sender: TObject);
begin
  DoReSize;
end;

procedure TCnColorGridPanel.PanelToolClick(Sender: TObject);
begin
  if Assigned(FPanelToolOnClick) then
     FPanelToolOnClick(Self);
end;

procedure TCnColorGridPanel.SetAutoSizeBy(Value: TAutoSizeBy);
begin
  FColorGrid.AutoSizeBy:=Value;
end;

procedure TCnColorGridPanel.SetColorGridBackGround(Value: TColor);
begin
  FColorGrid.Color:=Value;
end;

procedure TCnColorGridPanel.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    FToolBar.Images := Value;
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;
end;

procedure TCnColorGridPanel.SetToolPanelVisible(Value: Boolean);
begin
  if ToolPanelVisible<>Value then
  begin
    FPanelTool.Visible:=Value;
    DoReSize;
  end;
end;

initialization
  InitHSBColors;

end.
