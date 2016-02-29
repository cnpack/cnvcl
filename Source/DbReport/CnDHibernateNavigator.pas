{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
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

unit CnDHibernateNavigator; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准控件库
* 单元名称：数据导航控件单元
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
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
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Buttons,
  Graphics, CnDHibernateMemData, CnDHibernateSubQueryAdv, CnDHibernateQueryAdv,
  DBClient;

type
  TCnDHibernateNavBtn = (btnFirst, btnPrior, btnNext, btnLast);

  TCnDHibernateNavBtnSet = set of TCnDHibernateNavBtn;

  TCnDHibernateButtons = (dhbNone, dhbFirst, dhbPrior, dhbNext, dhbLast);

  TCnOnBtnClick = procedure(Sender: TObject; Button: TCnDHibernateButtons) of object;

  TCnDHibernateNavigator = class(TCustomPanel)
  private
    FBtns: array[TCnDHibernateNavBtn] of TBitBtn;
    FDHibernateDataSet: TClientDataSet;
    FOnBtnClick: TCnOnBtnClick;
    FIsSubDS: boolean;
    FButtonVisible: TCnDHibernateNavBtnSet;
    FAbout: string;
    function GetBtnFirstImage: TGraphic;
    function GetBtnLastImage: TGraphic;
    function GetBtnNextImage: TGraphic;
    function GetBtnPriorImage: TGraphic;
    procedure SetBtnFirstImage(const Value: TGraphic);
    procedure SetBtnLastImage(const Value: TGraphic);
    procedure SetBtnNextImage(const Value: TGraphic);
    procedure SetBtnPriorImage(const Value: TGraphic);
    procedure SetButtonVisible(const Value: TCnDHibernateNavBtnSet);
  protected
    procedure GenerateButtons;
    procedure DResize(Sender: TObject);
    procedure FBtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property About: string read FAbout write FAbout;
    property DHibernateDataSet: TClientDataSet read FDHibernateDataSet write FDHibernateDataSet;
    property BtnFirstImage: TGraphic read GetBtnFirstImage write SetBtnFirstImage;
    property BtnPriorImage: TGraphic read GetBtnPriorImage write SetBtnPriorImage;
    property BtnNextImage: TGraphic read GetBtnNextImage write SetBtnNextImage;
    property BtnLastImage: TGraphic read GetBtnLastImage write SetBtnLastImage;
    property OnBtnClick: TCnOnBtnClick read FOnBtnClick write FOnBtnClick;
    property IsSubDS: boolean read FIsSubDS write FIsSubDS default False;
    property ButtonVisible: TCnDHibernateNavBtnSet read FButtonVisible write SetButtonVisible default[btnFirst, btnPrior, btnNext, btnLast];
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{$R CnDHibernateNav.res}

{ TCnDHibernateNavigator }

constructor TCnDHibernateNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  FButtonVisible := [btnFirst, btnPrior, btnNext, btnLast];
  GenerateButtons;
  Self.OnResize := DResize;
  self.BevelInner := bvNone;
  Self.BevelOuter := bvNone;
  self.Caption := EmptyStr;
  FullRepaint := False;
end;

destructor TCnDHibernateNavigator.Destroy;
var
  i: TCnDHibernateNavBtn;
begin
  for i := Low(FBtns)to High(FBtns) do
    FBtns[i].Free;
  inherited Destroy;
end;

procedure TCnDHibernateNavigator.DResize(Sender: TObject);
var
  h, w: Integer;
  d: Integer;
  i: TCnDHibernateNavBtn;
  vc: Integer;           { visible count }
  n: Integer;
begin
  // 重设按钮的大小及位置
  d := 0;
  vc := 0;
  for i := Low(TCnDHibernateNavBtn)to High(TCnDHibernateNavBtn) do
  begin
    if i in FButtonVisible then
      Inc(vc);
    FBtns[i].Visible := i in FButtonVisible;
  end;
  h := self.Height;
  w := Self.Width;
  if vc > 0 then
  begin
    d := w div vc;
    w := d * vc;
  end;
  Self.Width := w;
  n := 0;
  for i := Low(FBtns)to High(FBtns) do
  begin
    if i in FButtonVisible then
    begin
      FBtns[i].Height := h;
      FBtns[i].Width := d;
      FBtns[i].BringToFront;
      FBtns[i].Left := n * d + 1;
      Inc(n);
    end;
  end;
  self.Repaint;
end;

procedure TCnDHibernateNavigator.FBtnClick(Sender: TObject);
var
  btn: TCnDHibernateButtons;
begin
  if FDHibernateDataSet = nil then
    raise Exception.Create('No data set found!');
  if not FDHibernateDataSet.Active then
    raise Exception.Create('Cannot do this operation on a close data set.'); 
  // click button
  btn := dhbNone;
  case TBitBtn(Sender).Tag of
    1:
      begin
        btn := dhbFirst;
        if FIsSubDS then
          TCnDHibernateSubQueryAdvance(FDHibernateDataSet).FirstPage
        else
          TCnDHibernateQueryAdvance(FDHibernateDataSet).FirstPage;
      end;
    2:
      begin
        btn := dhbPrior;
        if FIsSubDS then
          TCnDHibernateSubQueryAdvance(FDHibernateDataSet).PriorPage
        else
          TCnDHibernateQueryAdvance(FDHibernateDataSet).PriorPage;
      end;
    3:
      begin
        btn := dhbNext;
        if FIsSubDS then
          TCnDHibernateSubQueryAdvance(FDHibernateDataSet).NextPage
        else
          TCnDHibernateQueryAdvance(FDHibernateDataSet).NextPage;
      end;
    4:
      begin
        btn := dhbLast;
        if FIsSubDS then
          TCnDHibernateSubQueryAdvance(FDHibernateDataSet).LastPage
        else
          TCnDHibernateQueryAdvance(FDHibernateDataSet).LastPage;
      end;
  end;
  if Assigned(OnBtnClick) then
    OnBtnClick(Self, btn);
end;

procedure TCnDHibernateNavigator.GenerateButtons;
var
  i: TCnDHibernateNavBtn;
  n: Integer;
begin
  n := 0;
  for i := Low(FBtns)to High(FBtns) do
  begin
    FBtns[i] := TBitBtn.Create(Self);
    with FBtns[i] do
    begin
      Parent := Self;
      Tag := n + 1;
      OnClick := FBtnClick;
      Glyph.LoadFromResourceName(HInstance, Format('NAV_%d', [n + 1]));
      Visible := True;
    end;
    Inc(n);
  end;
  DResize(self);
end;

function TCnDHibernateNavigator.GetBtnFirstImage: TGraphic;
begin
  Result := FBtns[btnFirst].Glyph;
end;

function TCnDHibernateNavigator.GetBtnLastImage: TGraphic;
begin
  Result := FBtns[btnLast].Glyph;
end;

function TCnDHibernateNavigator.GetBtnNextImage: TGraphic;
begin
  Result := FBtns[btnNext].Glyph;
end;

function TCnDHibernateNavigator.GetBtnPriorImage: TGraphic;
begin
  Result := FBtns[btnPrior].Glyph;
end;

procedure TCnDHibernateNavigator.SetBtnFirstImage(const Value: TGraphic);
begin
  FBtns[btnFirst].Glyph.Assign(Value);
end;

procedure TCnDHibernateNavigator.SetBtnLastImage(const Value: TGraphic);
begin
  FBtns[btnLast].Glyph.Assign(Value);
end;

procedure TCnDHibernateNavigator.SetBtnNextImage(const Value: TGraphic);
begin
  FBtns[btnNext].Glyph.Assign(Value);
end;

procedure TCnDHibernateNavigator.SetBtnPriorImage(const Value: TGraphic);
begin
  FBtns[btnPrior].Glyph.Assign(Value);
end;

procedure TCnDHibernateNavigator.SetButtonVisible(const Value: TCnDHibernateNavBtnSet);
begin
  FButtonVisible := Value; 
  // todo: set visible
  DResize(self);
end; 

{$ENDIF SUPPORT_ADO}
end.
