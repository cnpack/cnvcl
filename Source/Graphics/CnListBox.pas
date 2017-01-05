{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2017 CnPack 开发组                       }
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

unit CnListBox;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：界面控件包自画 ListBox 实现单元
* 单元作者：匿名 + 网上代码移植
* 备    注：该界面效果类似于 CnWizards 的设置对话框中的列表框效果。
*           每行字符串中可用 \n 来控制换行
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.06.04 V0.1
*               实现单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, StdCtrls, Graphics, Windows;

type
  TCnListBox = class(TListBox)
  private
    FTextColor: TColor;
    FBackColor: TColor;
    FItemBackColor: TColor;
    FItemFrameColor: TColor;
    FSelectedBackColor: TColor;
    FSelectedTextColor: TColor;
    FImages: TImageList;
    FRoundWidth: Integer;
    FSelectedList: TStrings;
    FSubList: TStrings;
    function GetSelectedList: TStrings;
    procedure ListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SetImages(const Value: TImageList);
    procedure SetBackColor(const Value: TColor);
    procedure SetItemBackColor(const Value: TColor);
    procedure SetItemFrameColor(const Value: TColor);
    procedure SetSelectedBackColor(const Value: TColor);
    procedure SetSelectedTextColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
    procedure SetRoundWidth(const Value: Integer);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  public
    property SelectedList: TStrings read GetSelectedList;
    {* 多选时获得的选中的字符串列表}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RoundWidth: Integer read FRoundWidth write SetRoundWidth default 8;
    {* Item 自画的圆角矩形的圆角宽度，0 为不圆角}
    property BackColor: TColor Read FBackColor Write SetBackColor default clWhite;
    {* 背景颜色}
    property TextColor: TColor Read FTextColor Write SetTextColor default clBlack;
    {* 文字颜色}
    property ItemBackColor: TColor Read FItemBackColor Write SetItemBackColor default TColor($00FFF7F7);
    {* Item 的背景颜色}
    property ItemFrameColor: TColor Read FItemFrameColor Write SetItemFrameColor default TColor($00131315);
    {* Item 的边框颜色}
    property SelectedBackColor: TColor Read FSelectedBackColor Write SetSelectedBackColor default TColor($00FFB2B5);
    {* 选中的 Item 的背景颜色}
    property SelectedTextColor: TColor Read FSelectedTextColor Write SetSelectedTextColor default clBlue;
    {* 选中的 Item 的文字颜色}
    property Images: TImageList Read FImages Write SetImages;
    {* 外接的 ImageList，图标可供绘制}
  end;

implementation

procedure SplitString(Source, Deli: string; List: TStrings);
var
  EndOfCurrentString: byte;
begin
  List.Clear;
  while Pos(Deli, Source) > 0 do
  begin
    EndOfCurrentString := Pos(Deli, Source);
    List.Add(Copy(Source, 1, EndOfCurrentString - 1));
    Source := Copy(Source, EndOfCurrentString + Length(Deli),
      Length(Source) - EndOfCurrentString);
  end;
  List.Add(Source);
end;

{ TCnListBox }

constructor TCnListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style             := lbOwnerDrawFixed;
  Ctl3D             := False;
  ItemHeight        := 50;
  FRoundWidth       := 8;
  BackColor         := clWindow;
  Color             := BackColor;
  TextColor         := clBlack;
  ItemBackColor     := TColor($00FFF7F7);
  ItemFrameColor    := TColor($00131315);
  SelectedBackColor := TColor($00FFB2B5);
  SelectedTextColor := clBlue;
end;

destructor TCnListBox.Destroy;
begin
  FSelectedList.Free;
  FSubList.Free;
  inherited Destroy;
end;

procedure TCnListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
    ListDrawItem(Self, Index, Rect, State);
end;

function TCnListBox.GetSelectedList: TStrings;
begin
  if FSelectedList = nil then
    FSelectedList := TStringList.Create;

  if (Items.Count > 0) and (ItemIndex > -1) then
    SplitString(Items[ItemIndex], '\n', FSelectedList);

  Result := FSelectedList;
end;

procedure TCnListBox.ListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  useImage:      Boolean;
  itemLeft:      Integer;
  subItemHeight: Integer;
  i:             Integer;
begin
  Canvas.Brush.Color := BackColor;
  if Enabled then
    Canvas.Font.Color  := TextColor
  else
    Canvas.Font.Color := clGray;

  Canvas.FillRect(Rect);
  Canvas.Brush.Color := ItemBackColor;
  Canvas.Pen.Color   := ItemFrameColor;
  Canvas.RoundRect(Rect.Left + 3, Rect.Top + 3,
    Rect.Right - 2, Rect.Bottom - 2, FRoundWidth, FRoundWidth);
  Canvas.RoundRect(Rect.Left + 3, Rect.Top + 3,
    Rect.Right - 3, Rect.Bottom - 3, FRoundWidth, FRoundWidth);

  if (odSelected in State) then
  begin
    Canvas.Brush.Color := SelectedBackColor;
    Canvas.RoundRect(Rect.Left + 3, Rect.Top + 3,
      Rect.Right - 3, Rect.Bottom - 3, FRoundWidth, FRoundWidth);
    Canvas.Font.Color := SelectedTextColor;
    if (odFocused in State) then
      DrawFocusRect(Canvas.Handle, Rect);
  end;

  if Images = nil then
    useImage := False
  else
    useImage := True;

  if useImage then
    Images.Draw(
      Canvas,
      Rect.Left + 7,
      Rect.top + (ItemHeight - Images.Height) div 2,
      Index);

  if FSubList = nil then
    FSubList := TStringList.Create;
  SplitString(Items[index], '\n', FSubList);
  if useImage then
    itemLeft := Rect.Left + ItemHeight - 4
  else
    itemLeft := Rect.Left + 10;

  subItemHeight := (ItemHeight - 8) div FSubList.Count;

  for i := 0 to FSubList.Count - 1 do
  begin
    Canvas.TextOut(itemLeft, rect.Top + 4 + (i * subItemHeight),
      FSubList[i]);
  end;
end;

procedure TCnListBox.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TCnListBox.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TCnListBox.SetItemBackColor(const Value: TColor);
begin
  if FItemBackColor <> Value then
  begin
    FItemBackColor := Value;
    Invalidate;
  end;
end;

procedure TCnListBox.SetItemFrameColor(const Value: TColor);
begin
  if FItemFrameColor <> Value then
  begin
    FItemFrameColor := Value;
    Invalidate;
  end;
end;

procedure TCnListBox.SetRoundWidth(const Value: Integer);
begin
  if FRoundWidth <> Value then
  begin
    FRoundWidth := Value;
    Invalidate;
  end;    
end;

procedure TCnListBox.SetSelectedBackColor(const Value: TColor);
begin
  if FSelectedBackColor <> Value then
  begin
    FSelectedBackColor := Value;
    Invalidate;
  end;
end;

procedure TCnListBox.SetSelectedTextColor(const Value: TColor);
begin
  if FSelectedTextColor <> Value then
  begin
    FSelectedTextColor := Value;
    Invalidate;
  end;
end;

procedure TCnListBox.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Invalidate;
  end;
end;

end.
