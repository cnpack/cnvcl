{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnDataGrid;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：查询分析器组件中装载数据的网格控件
* 单元作者：佚名
*           不得闲 (appleak46@yahoo.com.cn)
* 备    注：
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.11.24 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  SysUtils, Classes, Controls, Grids, Windows, Forms, Messages, Graphics, Math,
  Adodb, DB, ADoInt, Dialogs,
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF}
  CnADOBinding, CnDBConsts;

type
  PRowData = ^TCnRowData;
  TCnRowData = array[0..0] of string;

  TCnDataRowList = class //数据行列表
  private
    FData: TList;
    FFieldCount: integer;//字段数
    FFieldNames: Array of string;//用来保存字段名字
    function AllocRowData: PRowData;
    procedure FreeRowData(P: PRowData);
    function GetFieldName(Index: Integer): string;
    procedure clear; //清除信息
  protected
  public
    Constructor Create;
    destructor Destroy; override;
    property Data: TList read FData;
    property FieldCount: integer read FFieldCount;
    property FieldName[Index: Integer]: string read GetFieldName;
    //使用给定的DataSet来填充行列表，Canceled指定用户是否取消了查询,普通的填充
    Function FillRowListByNormal(DataSet: TCustomADODataSet;var Canceled: Boolean): boolean;
    //使用微软的原生数据集进行绑定，全部交个OleDB自己去搞，速度要快些
    function FillRowListByBinding(DataSet: TCustomADODataSet;Var Canceled: Boolean): boolean;
    function FillRowList(DataSet: TCustomADoDataSet;Var Canceled: Boolean): boolean;
  end;

  TCnDataGrid = class(TDrawGrid)
  private
    FFlat: Boolean;
    FBorderWidth: integer;
    FOldTopRow: Integer;
    FDataSet: TCustomADODataSet;
    FSelectCellStr: string;
    FOnSelectCell: TSelectCellEvent;
    FAfterScroll: TDataSetNotifyEvent;
    FBeforeScroll: TDataSetNotifyEvent;
    FRows: TCnDataRowList;
    FDefaultMaxColumnWidth: Integer;
    procedure SetDataSet(DataSet: TCustomADODataSet);
    procedure SetFlat(const Value: Boolean);
    procedure UpdateIndicator;
    function GetString(ACol, ARow: Integer): string;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    function GetDataRowCount: Integer;
    function GetSelectionText: string;
    procedure AdjustColumns;
    procedure AdjustColumn(ACol: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure ChangeGridOrientation(RightToLeftOrientation: Boolean);
    procedure DrawBorder; virtual;//用来处理网格边框
    procedure Paint; override;
    function IsActiveControl: Boolean; //是否是活动控件
    procedure TopLeftChanged; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;//用来处理移动DataSet
    procedure CreateParams(var Params: TCreateParams); override;
    // 设置数据.
    procedure SetData{(Data: TCnDataRowList)};
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectCellStr: string read FSelectCellStr;

    // 数据行个数.
    property DataRowCount: Integer read GetDataRowCount;

    // 选中的文本. 字段之间用TAB分隔, 行之间用CRLF分隔.
    property SelectionText: string read GetSelectionText;

  published
    { Published declarations }
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Flat: Boolean read FFlat write SetFlat;
    property DataSet: TCustomADODataSet read FDataSet Write SetDataSet;
    property Font;
    property GridLineWidth;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectCell;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDataGrid }

procedure TCnDataGrid.AdjustColumn(ACol: Integer);
var
  R, R2, I, W1, W2, ColIndex: Integer;
  s: string;
begin
  if (ACol < 1) or (ACol > Self.ColCount) then Exit;

  // 设置列宽.
  R := Self.TopRow;
  R2 := R + Self.VisibleRowCount + 10;
  ColIndex := ACol - 1;

  W1 := 0;
  for I := R - 1 to R2 - 1 do
  begin
    if I >= FRows.Data.Count then Break;

    s := GetString(ColIndex, I);
    W2 := Canvas.TextWidth(s) + 5;
    if W2 > FDefaultMaxColumnWidth then
    begin
      W1 := FDefaultMaxColumnWidth;
      Break;
    end;

    W1 := Max(W1, W2);
  end;
  W2 := Canvas.TextWidth(FRows.FieldName[ColIndex]) + 5;
  Self.ColWidths[ACol] := Max(W1, W2);
end;

procedure TCnDataGrid.AdjustColumns;
var
  I: Integer;
begin
  Canvas.Font.Assign(Self.Font);
  for I := 1 to Self.ColCount-1 do
    AdjustColumn(I);
end;

procedure TCnDataGrid.ChangeGridOrientation(
  RightToLeftOrientation: Boolean);
var
  Org: TPoint;
  Ext: TPoint;
begin
  if RightToLeftOrientation then
  begin
    Org := Point(ClientWidth,0);
    Ext := Point(-1,1);
    SetMapMode(Canvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(Canvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(Canvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end
  else
  begin
    Org := Point(0,0);
    Ext := Point(1,1);
    SetMapMode(Canvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(Canvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(Canvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end;
end;

constructor TCnDataGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.DefaultRowHeight := 18;
  Self.ColCount := 2;
  Self.RowCount := 2;
  Self.FixedRows := 1;
  Self.FixedCols := 1;
  FRows := TCnDataRowList.Create;
  Self.Options := Self.Options + [goColSizing, goThumbTracking, goDrawFocusSelected];
  FDefaultMaxColumnWidth := 400;
end;

procedure TCnDataGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if Flat {and (Ctl3D = True)} then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
      if (BorderStyle = bsSingle) then
        FBorderWidth := 1 else FBorderWidth := 0;
    end else
      FBorderWidth := 0;
    Style := Style or WS_CLIPCHILDREN; //To avoid black box in Inplace editor When BufferedPaint.
  end;

end;

destructor TCnDataGrid.Destroy;
begin
  FRows.Free;
  inherited Destroy;
end;

{procedure TCnDataGrid.DoAfterScroll;
var
  tempbk,Bk: TBookmark;//用来保存用户移动之后的位置
  iRow: integer;
begin
  if assigned(FAfterScroll) then //执行用户函数
    FAfterScroll(FDataSet);
  //这里会搞成死循环，还是加个行列表进来算了，郁闷
 { Bk := FDataSet.GetBookmark;
  MoveFlag := true;
  if MoveFlag then
  begin
    FDataSet.First;
    iRow := 0;
    while not FDataSet.Eof do
    begin
       tempBk := FDataSet.GetBookmark;
       if tempBk <> Bk then  //比较两个的位置，如果不等则将行数加1
         iRow := iRow + 1
       else
         break;
       FDataSet.Next;
    end;
  end;
end;}

{procedure TCnDataGrid.DoBeforeScroll;
begin
  //MoveFlag := false;
  if assigned(FBeforeScroll) then //执行用户函数
    FBeforeScroll(FDataSet);
end; }

procedure TCnDataGrid.DrawBorder;
var
  DC: HDC;
  R: TRect;
begin
  if Flat and (BorderStyle = bsSingle) then
  begin  //BorderStyle为BsNone则应该为平面
    DC := GetWindowDC(Handle);
    try
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);//指定矩形按指定偏移
      //DrawEdge(DC, R,BDR_SUNKENOUTER, BF_TOPLEFT);
      //DrawEdge(DC, R,BDR_SUNKENOUTER, BF_BOTTOMRIGHT);
      DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT);//该函数用来实现绘制三维效果
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TCnDataGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  I, J: Integer;
  S: string;
begin
  Inc(ARect.Left, 3);

  if (ARow = 0) and (ACol > 0) then
  begin
    // 标题.
    I := ACol - 1;
    if I < FRows.FieldCount then
      S := FRows.FieldName[I] //得到该列的标题
    else
    begin
      inherited;
      Exit;
    end;
  end
  else if (ACol = 0) and (ARow > 0) and (FRows.Data.Count > 0) then
  begin
    // Indicator.
    S := IntToStr(ARow); //写第一列中的数字
  end
  else
  begin
    J := ARow - 1; //因为前面多一个fixCol和一个FixRow，而i,j从0开始
    I := ACol - 1;
    if (J < 0) or (I < 0) or (J >= FRows.Data.Count) or (I >= FRows.FieldCount) then
    begin
      inherited;
      Exit;
    end;
    S := GetString(I, J); //否则就得到对应位置上的值
  end;

  DrawText(Canvas.Handle, PChar(S), Length(S), ARect,
            DT_VCENTER or DT_SINGLELINE or DT_LEFT);//开始写字
end;

function TCnDataGrid.GetDataRowCount: Integer;
begin
  Result := FRows.Data.Count;
end;

function TCnDataGrid.GetSelectionText: string;
var
  GridRect: TGridRect;
  R, C: Integer;
  S: string;
begin
  GridRect := Self.Selection;
  for R := GridRect.Top - 1 to GridRect.Bottom - 1 do
  begin
    for C := GridRect.Left - 1 to GridRect.Right - 1 do
    begin
      if C >= GridRect.Left then S := S + #9;
      S := S + GetString(C, R);
    end;
    S := S + #13#10;
  end;
  Result := S;

end;

function TCnDataGrid.GetString(ACol, ARow: Integer): string;
var
  RowData: PRowData;
begin
  RowData := FRows.Data[ARow];
  Result := RowData[ACol];
end;

function TCnDataGrid.IsActiveControl: Boolean;
var
  H: Hwnd;
  ParentForm: TCustomForm;
begin
  Result := False;
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
  begin
    if (ParentForm.ActiveControl = Self) then
      Result := True
  end
  else
  begin
    H := GetFocus;
    while IsWindow(H) and not Result do
    begin
      if H = WindowHandle then
        Result := True
      else
        H := GetParent(H);
    end;
  end;
end;

procedure TCnDataGrid.Paint;
type
//  TPointArray = array of TPoint;
  TIntArray = array of Integer;
var
  LineColor: TColor;
  DrawInfo: TGridDrawInfo;
  Sel: TGridRect;
  UpdateRect: TRect;
  AFocRect, FocRect: TRect;
  PointsList: TIntArray;
  StrokeList: TIntArray;
  I: Integer;
  MaxStroke: Integer;
  FrameFlags1, FrameFlags2: DWORD;
  FixedLineColor: TColor;

  procedure DrawLines(DoHorz, DoVert: Boolean; Col, Row: Longint;
    const CellBounds: array of Integer; OnColor, OffColor: TColor);

  { Cellbounds is 4 integers: StartX, StartY, StopX, StopY
    Horizontal lines:  MajorIndex = 0
    Vertical lines:    MajorIndex = 1 }

  const
    FlatPenStyle = PS_Geometric or PS_Solid or PS_EndCap_Flat or PS_Join_Miter;

    procedure DrawAxisLines(const AxisInfo: TGridAxisDrawInfo;
      Cell, MajorIndex: Integer; UseOnColor: Boolean);
    var
      Line: Integer;
      LogBrush: TLOGBRUSH;
      Index: Integer;
      Points: TIntArray;
      StopMajor, StartMinor, StopMinor, StopIndex: Integer;
      LineIncr: Integer;
    begin
      Points := nil;
      with Canvas, AxisInfo do
      begin
        if EffectiveLineWidth <> 0 then
        begin
          Pen.Width := GridLineWidth;
          if UseOnColor then
            Pen.Color := OnColor
          else
            Pen.Color := OffColor;
          if Pen.Width > 1 then
          begin
            LogBrush.lbStyle := BS_Solid;
            LogBrush.lbColor := Pen.Color;
            LogBrush.lbHatch := 0;
            Pen.Handle := ExtCreatePen(FlatPenStyle, Pen.Width, LogBrush, 0, nil);
          end;
          Points := PointsList;
          Line := CellBounds[MajorIndex] + EffectiveLineWidth shr 1 +
            GetExtent(Cell);

          if UseRightToLeftAlignment and (MajorIndex = 0) then Inc(Line);
          StartMinor := CellBounds[MajorIndex xor 1];
          StopMinor := CellBounds[2 + (MajorIndex xor 1)];
          StopMajor := CellBounds[2 + MajorIndex] + EffectiveLineWidth;
          StopIndex := MaxStroke * 4;
          Index := 0;
          repeat
            Points[Index + MajorIndex] := Line;         { MoveTo }
            Points[Index + (MajorIndex xor 1)] := StartMinor;
            Inc(Index, 2);
            Points[Index + MajorIndex] := Line;         { LineTo }
            Points[Index + (MajorIndex xor 1)] := StopMinor;
            Inc(Index, 2);

            repeat
              Inc(Cell);
              LineIncr := GetExtent(Cell) + EffectiveLineWidth;
            until (LineIncr > 0) or (Cell > LastFullVisibleCell);
            Inc(Line, LineIncr);
          until (Line > StopMajor) or (Cell > LastFullVisibleCell) or (Index > StopIndex);
           { 2 integers per point, 2 points per line -> Index div 4 }
          PolyPolyLine(Canvas.Handle, Pointer(Points)^, Pointer(StrokeList)^, Index shr 2);
        end;
      end;
    end;

  begin
    if (CellBounds[0] = CellBounds[2]) or (CellBounds[1] = CellBounds[3]) then Exit;
    if not DoHorz then
    begin
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
    end
    else
    begin
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
    end;
  end;

  function PointInGridRect(Col, Row: Longint; const Rect: TGridRect): Boolean;
  begin
    Result := (Col >= Rect.Left) and (Col <= Rect.Right) and (Row >= Rect.Top)
      and (Row <= Rect.Bottom);
  end;

  procedure DrawCells(ACol, ARow: Longint; StartX, StartY, StopX, StopY: Integer;
    Color: TColor; IncludeDrawState: TGridDrawState);
  var
    CurCol, CurRow: Longint;
    AWhere, Where, TempRect: TRect;
    DrawState: TGridDrawState;
    Focused: Boolean;
  begin
    CurRow := ARow;
    Where.Top := StartY;
    while (Where.Top < StopY) and (CurRow < RowCount) do
    begin
      CurCol := ACol;
      Where.Left := StartX;
      Where.Bottom := Where.Top + RowHeights[CurRow];
      while (Where.Left < StopX) and (CurCol < ColCount) do
      begin
        Where.Right := Where.Left + ColWidths[CurCol];
        if (Where.Right > Where.Left) and RectVisible(Canvas.Handle, Where) then
        begin
          DrawState := IncludeDrawState;
          Focused := IsActiveControl;
          if Focused and (CurRow = Row) and (CurCol = Col)  then
            Include(DrawState, gdFocused);
          if PointInGridRect(CurCol, CurRow, Sel) then
            Include(DrawState, gdSelected);
          if not (gdFocused in DrawState) or not (goEditing in Options) or
            not EditorMode or (csDesigning in ComponentState) then
          begin
            if DefaultDrawing or (csDesigning in ComponentState) then
              with Canvas do
              begin
                Font := Self.Font;
                if (gdSelected in DrawState) and
                  (not (gdFocused in DrawState) or
                  ([goDrawFocusSelected, goRowSelect] * Options <> [])) then
                begin
                  Brush.Color := clHighlight;
                  Font.Color := clHighlightText;
                end
                else
                  Brush.Color := Color;
                FillRect(Where);
              end;
            DrawCell(CurCol, CurRow, Where, DrawState);
            if DefaultDrawing and (gdFixed in DrawState) and Ctl3D and
              ((FrameFlags1 or FrameFlags2) <> 0) then
            begin
              TempRect := Where;
              if (FrameFlags1 and BF_RIGHT) = 0 then
                Inc(TempRect.Right, DrawInfo.Horz.EffectiveLineWidth)
              else if (FrameFlags1 and BF_BOTTOM) = 0 then
                Inc(TempRect.Bottom, DrawInfo.Vert.EffectiveLineWidth);
              DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags1);
              DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags2);
            end;

            if DefaultDrawing and not (csDesigning in ComponentState) and
              (gdFocused in DrawState) and
              ([goEditing, goAlwaysShowEditor] * Options <>
              [goEditing, goAlwaysShowEditor])
              and not (goRowSelect in Options) then
            begin
              if not UseRightToLeftAlignment then
                DrawFocusRect(Canvas.Handle, Where)
              else
              begin
                AWhere := Where;
                AWhere.Left := Where.Right;
                AWhere.Right := Where.Left;
                DrawFocusRect(Canvas.Handle, AWhere);
              end;
            end;
          end;
        end;
        Where.Left := Where.Right + DrawInfo.Horz.EffectiveLineWidth;
        Inc(CurCol);
      end;
      Where.Top := Where.Bottom + DrawInfo.Vert.EffectiveLineWidth;
      Inc(CurRow);
    end;
  end;

begin
  if UseRightToLeftAlignment then
    ChangeGridOrientation(True);

  if Flat then
    FixedLineColor := clBtnShadow
  else
    FixedLineColor := clBlack;

  UpdateRect := Canvas.ClipRect;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    if (Horz.EffectiveLineWidth > 0) or (Vert.EffectiveLineWidth > 0) then
    begin
      { Draw the grid line in the four areas (fixed, fixed), (variable, fixed),
        (fixed, variable) and (variable, variable) }
      LineColor := clSilver;
      MaxStroke := Max(Horz.LastFullVisibleCell - LeftCol + FixedCols,
                        Vert.LastFullVisibleCell - TopRow + FixedRows) + 3;
      SetLength(PointsList, MaxStroke * 2 * 2);
      SetLength(StrokeList, MaxStroke);
      for I := 0 to MaxStroke - 1 do
        StrokeList[I] := 2;

      if ColorToRGB(Color) = clSilver then LineColor := clGray;
      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        0, 0, [0, 0, Horz.FixedBoundary, Vert.FixedBoundary], FixedLineColor, FixedColor);
      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        LeftCol, 0, [Horz.FixedBoundary, 0, Horz.GridBoundary,
        Vert.FixedBoundary], FixedLineColor, FixedColor);
      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        0, TopRow, [0, Vert.FixedBoundary, Horz.FixedBoundary,
        Vert.GridBoundary], FixedLineColor, FixedColor);
      DrawLines(goHorzLine in Options, goVertLine in Options, LeftCol,
        TopRow, [Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridBoundary,
        Vert.GridBoundary], LineColor, Color);

      SetLength(StrokeList, 0);
      SetLength(PointsList, 0);
    end;

    { Draw the cells in the four areas }
    Sel := Selection;
    FrameFlags1 := 0;
    FrameFlags2 := 0;
    if goFixedVertLine in Options then
    begin
      if not Flat then FrameFlags1 := BF_RIGHT;
      FrameFlags2 := BF_LEFT;
    end;
    if goFixedHorzLine in Options then
    begin
      if not Flat then FrameFlags1 := FrameFlags1 or BF_BOTTOM;
      FrameFlags2 := FrameFlags2 or BF_TOP;
    end;
    DrawCells(0, 0, 0, 0, Horz.FixedBoundary, Vert.FixedBoundary, FixedColor,
      [gdFixed]);
    DrawCells(LeftCol, 0, Horz.FixedBoundary {- FColOffset}, 0, Horz.GridBoundary,  //!! clip
      Vert.FixedBoundary, FixedColor, [gdFixed]);
    DrawCells(0, TopRow, 0, Vert.FixedBoundary, Horz.FixedBoundary,
      Vert.GridBoundary, FixedColor, [gdFixed]);
    DrawCells(LeftCol, TopRow, Horz.FixedBoundary {- FColOffset},                   //!! clip
      Vert.FixedBoundary, Horz.GridBoundary, Vert.GridBoundary, Color, []);

    if not (csDesigning in ComponentState) and
      (goRowSelect in Options) and DefaultDrawing and Focused then
    begin
//      GridRectToScreenRect(GetSelection, FocRect, False);
      with Selection do
        FocRect := BoxRect(Left, Top, Right, Bottom);

      if not UseRightToLeftAlignment then
        Canvas.DrawFocusRect(FocRect)
      else
      begin
        AFocRect := FocRect;
        AFocRect.Left := FocRect.Right;
        AFocRect.Right := FocRect.Left;
        DrawFocusRect(Canvas.Handle, AFocRect);
      end;
    end;

    { Fill in area not occupied by cells }
    if Horz.GridBoundary < Horz.GridExtent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent, Vert.GridBoundary));
    end;
    if Vert.GridBoundary < Vert.GridExtent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(0, Vert.GridBoundary, Horz.GridExtent, Vert.GridExtent));
    end;
  end;

  if UseRightToLeftAlignment then ChangeGridOrientation(False);
end;

function TCnDataGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  result := true;
  if Assigned(FOnSelectCell) then FOnSelectCell(Self, ACol, ARow, Result);
end;

procedure TCnDataGrid.SetData{(Data: TCnDataRowList)};
begin
  if FRows <> nil then
  begin
    //if FRows <> nil then FRows.Free;
    //FRows := Data;
    Self.ColCount := FRows.FieldCount + 1;
    if FRows.Data.Count = 0 then
      Self.RowCount := 2
    else
      Self.RowCount := FRows.Data.Count + 1;

    Self.ColWidths[0] := 25;
    Self.FixedCols := 1;
    Self.FixedRows := 1;
    AdjustColumns;
  end
  else
  begin
    Self.FixedCols := 0;
    Self.FixedRows := 0;
    Self.RowCount := 1;
    Self.ColCount := 1;
  end;
end;

procedure TCnDataGrid.SetDataSet(DataSet: TCustomADODataSet);
var
  Cancle: boolean;
begin
  if FDataSet <> DataSet then
  begin
    FDataSet := DataSet;
    FAfterScroll := DataSet.AfterScroll; //用来保留滚动后函数指针保存
    FBeforeScroll := DataSet.BeforeScroll; //滚动前函数指针保存
    cancle := false;
    FRows.FillRowList(DataSet,Cancle);
    self.SetData;
  end;
end;

procedure TCnDataGrid.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    RecreateWnd();
  end;
end;

procedure TCnDataGrid.TopLeftChanged;
begin
  inherited;
  UpdateIndicator;
end;

procedure TCnDataGrid.UpdateIndicator;
var
  BottomRow, w: Integer;
begin
  if FOldTopRow <> TopRow then
  begin
    FOldTopRow := TopRow;
    BottomRow := FOldTopRow + Self.VisibleRowCount;
    w := Canvas.TextWidth(IntToStr(BottomRow));
    Inc(W, 5);
    if w < 25 then w := 25;
    Self.ColWidths[0] := w;
  end;
end;

procedure TCnDataGrid.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;

  with Message.CalcSize_Params^ do
    InflateRect(rgrc[0], -FBorderWidth, -FBorderWidth);
end;

procedure TCnDataGrid.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  DrawBorder;
end;

{ TCnDataRowList }

function TCnDataRowList.AllocRowData: PRowData;
begin
  Result := AllocMem(FFieldCount * SizeOf(Pointer));//分配内存空间且初始化为0
end;

constructor TCnDataRowList.Create;
begin
  FData := TList.Create;
end;

destructor TCnDataRowList.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function TCnDataRowList.FillRowListByNormal(DataSet: TCustomADODataSet;
  var Canceled: Boolean): boolean;
var
  Value: OleVariant;
  P: PRowData;
  i: integer;
  function IsCanceled: Boolean;
  begin
    Result := Canceled;
  end;
begin
  result := true;
  while not DataSet.Eof do
  begin
    P := AllocRowData;
    for i := 0 to FFieldCount - 1 do
    begin
      Value := DataSet.Fields[i].Value;
      if VarIsNull(Value) then
        P[i] := 'NULL'
      else if VarIsArray(Value) then
        P[i] := '(Blob)'
      else
        P[i] := string(Value);
    end;
    FData.Add(P);
    if IsCanceled then
    begin
      Result := False;
      Exit;
    end;
    DataSet.Next;
  end;
end;

function TCnDataRowList.FillRowListByBinding(DataSet: TCustomADODataSet;
  var Canceled: Boolean): boolean;
var
  Binding: TADOBinding;
  I: Integer;
  Fld: Field;
  FieldData: PColumnRawData;
  P: PRowData;
  s: string;
  FldTypes: array of Integer;
  M,N: Integer;
  Rst: _RecordSet;//原生数据记录集
  function IsCanceled: Boolean;
  begin
    Result := Canceled;
  end;
begin
  Result := True;
  Rst := DataSet.Recordset;
  Binding := TADOBinding.Create;
  try
    SetLength(FldTypes, FFieldCount);
    // set bindings
    for I := 0 to FFieldCount - 1 do
    begin
      Fld := Rst.Fields[I];
      FldTypes[I] := Fld.Type_;
      // 一律转化为string, 让OLE DB内部去搞吧.
      case Fld.Type_ of
        adLongVarChar,
        adLongVarWChar,
        adLongVarBinary,
        adBinary,
        adVarBinary,
        adBSTR,
        adChar,
        adVarChar,
        adWChar,
        adVarWChar:
          Binding.AddBinding(I + 1, adVarChar, 256 + 1, False);

//        adBSTR,
//        adChar,
//        adVarChar:
//          Binding.AddBinding(I + 1, adVarChar, Fld.DefinedSize + 1, False);
//
//        adWChar,
//        adVarWChar:
//          Binding.AddBinding(I + 1, adVarChar, Fld.DefinedSize + 1, False);

      else
        Binding.AddBinding(I + 1, adVarChar, 50 + 1, False);
      end;
    end;

    (Rst as IADORecordBinding).BindToRecordset(Binding.GetADOBindingData);
    M := 0;
    while not Rst.EOF do
    begin
      P := AllocRowData;
      N := 0;
      for I := 0 to FFieldCount - 1 do
      begin
        FieldData := Binding.AsRawData[I];
        if not (FieldData.Status in [adFldOK, adFldNull, adFldTruncated, adFldDefault]) then
          raise Exception.Create('ADO.ISqlW: get field data error.');// 假如出错, 应该是OLE DB不能处理的一些数据转换.
        if FieldData.Status = adFldNull then
          P[I] := 'NULL'
        else begin
          try
            case FldTypes[I] of
            adLongVarChar,
            adLongVarWChar,
            adLongVarBinary,
            adBinary,
            adVarBinary,
            adBSTR,
            adChar,
            adVarChar,
            adWChar,
            adVarWChar:
              if FieldData.DataLength > 256 then
                FieldData.DataLength := 256;
            else
            if FieldData.DataLength > 50 then
              FieldData.DataLength := 50;
            end;
          SetString(s, PChar(@FieldData.RawData), FieldData.DataLength);
          if (FldTypes[I] = adLongVarBinary) or
            (FldTypes[I] = adVarBinary) or (FldTypes[I] = adBinary)
          then s := '0x' + s;

          P[I] := s;
          except  // 好像还有问题, 姑且留着
            s := Format('Row:%d, Field:%d, Len: %d, Stat: %d', [M, N, FieldData.DataLength, FieldData.Status]);
            raise Exception.Create(s);
          end;
        end;
        Inc(N);
      end;

      FData.Add(P);

      if IsCanceled then
      begin
        Result := False;
        Exit;
      end;
      Rst.MoveNext;
      Inc(M);
    end;
  finally
    (Rst as IADORecordBinding).BindToRecordset(nil);
    Binding.Free;
  end;

end;

procedure TCnDataRowList.FreeRowData(P: PRowData);
var
  I: Integer;
begin
  for I := 0 to FFieldCount - 1 do
    P[I] := '';
end;

function TCnDataRowList.GetFieldName(Index: Integer): string;
begin
 if (Index < 0) or (Index >= FFieldCount) then
    raise Exception.Create(SCnIndexOut);

  Result := FFieldNames[Index];   //得到第i个字段的字段名字
end;

function TCnDataRowList.FillRowList(DataSet: TCustomADoDataSet;
  var Canceled: Boolean): boolean;
var
  I: Integer;
//  Ver: Double;
  Rst: _ReCordSet;

  function IsCanceled: Boolean;
  begin
    Result := Canceled;
  end;

begin
  Result := True;
  // 清除旧的数据.
  Clear;
  Rst := DataSet.Recordset;
  if (Rst <> nil) and ((Rst.State and adStateOpen) = adStateOpen) then
  begin
    // 创建新的
    FFieldCount := Rst.Fields.Count;
    SetLength(FFieldNames, FFieldCount);
    for I := 0 to Rst.Fields.Count-1 do
      FFieldNames[I] := Rst.Fields[I].Name;

    if Rst.Supports(adMovePrevious) then Rst.MoveFirst;

    //Ver := StrToFloat(DataSet.Connection.Version);
    //if Ver >= 2.0 then // 用ADO Binding效率会高一些.
    //  result := FillRowListByBinding(DataSet,Canceled)
    //else
    Result := self.FillRowListByNormal(DataSet,Canceled)
  end;

end;

procedure TCnDataRowList.clear;
var
  I: Integer;
  P: PRowData;
begin
  for I := 0 to FData.Count-1 do
  begin
    P := FData[I];
    FreeRowData(P);
    FreeMem(P);
  end;
end;

{$ENDIF SUPPORT_ADO}
end.
