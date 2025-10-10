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

unit CnTabSet;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�������˫���¼���TabSetʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2017.12.11
*             ������� Tab �� Hint���������ڲ�ͬ Tab ���Զ��л�
*           2016.05.23
*             ���� Tab ���ɼ�ʱ��������һ��ķ���
*           2007.03.06
*             ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, Graphics, Forms, Tabs;

type
  TCnTabSetCloseEvent = procedure(Sender: TObject; Index: Integer;
    var CanClose: Boolean) of object;

  TCnTabSetTabHintEvent = procedure(Sender: TObject; Index: Integer;
    var HintStr: string) of object;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnTabSet = class(TTabSet)
  private
    FDblClickClose: Boolean;
    FOnCloseTab: TCnTabSetCloseEvent;
    FShowTabHint: Boolean;
    FOnTabHint: TCnTabSetTabHintEvent;
    function CalcVisibleTabs(Start, Stop: Integer; Canvas: TCanvas;
      First: Integer): Integer;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    procedure DoCloseTab(Index: Integer; var CanClose: Boolean); virtual;
  public
    procedure MakeTabVisible;
    {* ��ǰ�� Tab ��ʾʱ��������һ�� Tab ��}
  published
    property DblClickClose: Boolean read FDblClickClose write FDblClickClose;
    {* �Ƿ�˫��ʱ�Զ��رյ�ǰҳ��}
    property OnCloseTab: TCnTabSetCloseEvent read FOnCloseTab write FOnCloseTab;
    {* ˫��ʱ�Զ��ر�ҳ��ǰ�������¼�}
    property ShowTabHint: Boolean read FShowTabHint write FShowTabHint;
    {* �Ƿ���� Tab ��ʾ Hint}
    property OnTabHint: TCnTabSetTabHintEvent read FOnTabHint write FOnTabHint;
    {* Tab Ҫ��ʾ Hint ʱ�������¼�}
    property OnDblClick;
    {* ˫��ʱ�����¼�}
  end;

implementation

const
  EdgeWidth = 9;

{ TCnTabSet }

function TCnTabSet.CalcVisibleTabs(Start, Stop: Integer; Canvas: TCanvas;
  First: Integer): Integer;
var
  Index, ASize: Integer;
  W: Integer;
begin
  Index := First;
  while (Start < Stop) and (Index < Tabs.Count) do
    with Canvas do
    begin
      W := TextWidth(Tabs[Index]);

      if (Style = tsOwnerDraw) then MeasureTab(Index, W);

      ASize := W;
      Inc(Start, ASize + EdgeWidth);    { next usable position }

      if Start <= Stop then
      begin
        Inc(Index);
      end;
    end;
  Result := Index - First;
end;

procedure TCnTabSet.CMHintShow(var Message: TMessage);
var
  P: TPoint;
  Index: Integer;
  S: string;
begin
  Message.Result := 1;
  P := ScreenToClient(Mouse.CursorPos);
  Index := ItemAtPos(P) + FirstIndex;

  if (Index >= 0) and Assigned(FOnTabHint) then
  begin
    S := Hint;
    FOnTabHint(Self, Index, S);
    if S <> '' then
    begin
      TCMHintShow(Message).HintInfo^.HintStr := S;
      Message.Result := 0;
    end;
  end;
end;

procedure TCnTabSet.DoCloseTab(Index: Integer; var CanClose: Boolean);
begin
  if Assigned(FOnCloseTab) then
    FOnCloseTab(Self, Index, CanClose);
end;

procedure TCnTabSet.MakeTabVisible;
var
  VTC: Integer;
begin
  // �����ǰ�޿ɼ� Tab����������ʼ
  VTC := CalcVisibleTabs(StartMargin + EdgeWidth, Width - EndMargin,
    Canvas, FirstIndex);
  if VTC = 0 then
    FirstIndex := 0;
end;

procedure TCnTabSet.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  P: TPoint;
  Index: Integer;
  CanClose: Boolean;
begin
  inherited;
  DblClick;

  if not FDblClickClose then
    Exit;

  P := ScreenToClient(Mouse.CursorPos);
  Index := ItemAtPos(P) + FirstIndex;

  if Index >= 0 then
  begin
    CanClose := True;
    DoCloseTab(Index, CanClose);
    
    if CanClose then
    begin
      Tabs.Delete(Index);
      MakeTabVisible;
    end;
  end;
end;

end.
