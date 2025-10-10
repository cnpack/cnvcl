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

unit CnListBox;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�����ؼ����Ի� ListBox ʵ�ֵ�Ԫ
* ��Ԫ���ߣ����� + ���ϴ�����ֲ
* ��    ע���ý���Ч�������� CnWizards �����öԻ����е��б��Ч����
*           ÿ���ַ����п��� \n �����ƻ���
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.06.04 V0.1
*               ʵ�ֵ�Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, StdCtrls, Graphics, Windows;

type
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
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
    {* ��ѡʱ��õ�ѡ�е��ַ����б�}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RoundWidth: Integer read FRoundWidth write SetRoundWidth default 8;
    {* Item �Ի���Բ�Ǿ��ε�Բ�ǿ�ȣ�0 Ϊ��Բ��}
    property BackColor: TColor Read FBackColor Write SetBackColor default clWhite;
    {* ������ɫ}
    property TextColor: TColor Read FTextColor Write SetTextColor default clBlack;
    {* ������ɫ}
    property ItemBackColor: TColor Read FItemBackColor Write SetItemBackColor default TColor($00FFF7F7);
    {* Item �ı�����ɫ}
    property ItemFrameColor: TColor Read FItemFrameColor Write SetItemFrameColor default TColor($00131315);
    {* Item �ı߿���ɫ}
    property SelectedBackColor: TColor Read FSelectedBackColor Write SetSelectedBackColor default TColor($00FFB2B5);
    {* ѡ�е� Item �ı�����ɫ}
    property SelectedTextColor: TColor Read FSelectedTextColor Write SetSelectedTextColor default clBlue;
    {* ѡ�е� Item ��������ɫ}
    property Images: TImageList Read FImages Write SetImages;
    {* ��ӵ� ImageList��ͼ��ɹ�����}
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
