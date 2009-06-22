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

unit CnQQPanel;
{* |<PRE>
================================================================================
* 软件名称：界面组件包
* 单元名称：QQ面板显示界面的实现单元
* 单元作者：rarnu(rarnu@cnpack.org)
* 备    注：
* 开发平台：Windows2003 Server + Delphi2007 up2
* 兼容测试：Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnQQPanel.pas,v 1.3 2009/01/02 08:27:39 liuxiao Exp $
* 修改记录：2009.06.22 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, SysUtils, StdCtrls, Graphics, ExtCtrls, Controls, Forms;

type
  TCnMemberNotifyEvent = procedure(Sender: TObject; AData: Pointer) of object;

  TCnIconNotifyEvent = procedure(Sender: TObject; AData: Pointer) of object;

  TCnQQIconData = class
  private
    FIconDesc: string;
    FIconName: string;
    FIconID: string;
  public
    property IconID: string read FIconID write FIconID;
    property IconName: string read FIconName write FIconName;
    property IconDesc: string read FIconDesc write FIconDesc;
  end;
  
  TCnQQIco = class(TPanel)
  private
    FImage: TImage;
    FData: Pointer;
    FOnIconClick: TCnIconNotifyEvent;
    FOnIconDoubleClick: TCnIconNotifyEvent;
    FNormalIcon: string;
    FHotIcon: string;
    procedure SetNormalIcon(const Value: string);
    procedure SetData(const Value: Pointer);
  protected
    procedure OnImgClick(Sender: TObject);
    procedure OnImgDblClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  public
    property Data: Pointer read FData write SetData;
    property Image: TImage read  FImage write FImage;
    property NormalIcon: string read FNormalIcon write SetNormalIcon;
    property HotIcon: string read FHotIcon write FHotIcon;
    property OnIconClick: TCnIconNotifyEvent read FOnIconClick write FOnIconClick;
    property OnIconDoubleClick: TCnIconNotifyEvent read FOnIconDoubleClick write FOnIconDoubleClick;
  end;

  TCnQQIcoArray = array of TCnQQIco;

  TCnQQPerson = class
  private
    FUserID: string;
    FUserDesc: string;
    FUserName: string;
    FUserIcons: TCnQQIcoArray;
    FUserHead: string;
    FNameColor: TColor;
  public
    procedure AddIcon(Ico: TCnQQIco);
    procedure RemoveIcon(Index: Integer);
    constructor Create;
  public
    property UserID: string read FUserID write FUserID;
    property UserName: string read FUserName write FUserName;
    property UserDesc: string read FUserDesc write FUserDesc;
    property UserIcons: TCnQQIcoArray read FUserIcons write FUserIcons;
    property UserHead: string read FUserHead write FUserHead;
    property NameColor: TColor read FNameColor write FNameColor default clBlack;
  end;

  TCnQQMember = class(TPanel)
  private
    FHeadImage: TImage;
    FPnlCont: TPanel;
    FPNickName: TPanel;
    FPDesc: TPanel;
    FPExtension: TPanel;
    { FGlass: TRaNGlassPanel; }
    FNickName: TLabel;
    FDesc: TLabel;
    FExtension: TLabel;
    FData: Pointer;
    FQQIcos: TCnQQIcoArray;
    FUserID: string;
  protected
    procedure OnGlassMouseEnter(Sender: TObject);
    procedure OnGlassMouseLeave(Sender: TObject);
    procedure OnGlassClick(Sender: TObject);
    procedure OnGlassDoubleClick(Sender: TObject);

    procedure OnImageMouseEnter(Sender: TObject);
    procedure OnImageMouseLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddIcon(Ico: TCnQQIco);
    procedure RemoveIcon(Index: Integer);
  public
    property Data: Pointer read FData write FData;
    property QQIcons: TCnQQIcoArray read  FQQIcos write FQQIcos;
  published
    property UserID: string read FUserID write FUserID;
    property HeadImage: TImage read FHeadImage write FHeadImage;
    property NickName: TLabel read FNickName write FNickName;
    property Desc: TLabel read FDesc write FDesc;
    property Extension: TLabel read FExtension write FExtension;
  end;

  TCnQQMemberArray = array of TCnQQMember;

  TCnQQGroup = class(TPanel)
  private
    FMembers: TCnQQMemberArray;
    function GetMemberCount: Integer;
    procedure SetMemberCount(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function AddMember(person: TCnQQPerson): Boolean;
    procedure RemoveMember(Index: Integer);
  public
    property Members: TCnQQMemberArray read FMembers write FMembers;
  published
    property MemberCount: Integer read GetMemberCount write SetMemberCount;
  end;

  TCnQQGroupWTitle = class(TPanel)
  private
    FTitle: TPanel;
    FGroup: TCnQQGroup;
    FTitleImage: TImage;
    FTitleName: TLabel;
  protected
    procedure OnTitleClick(Sender: TObject);
    procedure OnTitleMouseEnter(Sender: TObject);
    procedure OnTitleMouseLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Expand;
    procedure Packup;
  published
    property Title: TPanel read FTitle write FTitle;
    property TitleImage: TImage read FTitleImage write FTitleImage;
    property TitleName: TLabel read FTitleName write FTitleName;
    property QQGroup: TCnQQGroup read FGroup write FGroup;
  end;

  TCnQQGroupWTitleArray = array of TCnQQGroupWTitle;

  TCnQQPanel = class(TScrollBox)
  private
    FGroups: TCnQQGroupWTitleArray;
    FOnMemberClick: TCnMemberNotifyEvent;
    FOnMemberDblClick: TCnMemberNotifyEvent;
    function GetGroupCount: Integer;
    procedure SetGroupCount(const Value: Integer);
  protected
    procedure SetPanelHeight;
  public
    constructor Create(AOwner: TComponent); override;
    function AddGroup(AName: string): boolean;
    procedure RemoveGroup(Index: Integer);
    procedure ExpandAll;
    procedure PackupAll;
  public
    property Groups: TCnQQGroupWTitleArray read FGroups write FGroups;
  published
    property GroupCount: Integer read GetGroupCount write SetGroupCount;
    property OnMemberClick: TCnMemberNotifyEvent read FOnMemberClick write FOnMemberClick;
    property OnMemberDblClick: TCnMemberNotifyEvent read FOnMemberDblClick write FOnMemberDblClick;
  end;

implementation

{$R CnQQPanel.res}

{ TCnQQMember }

procedure TCnQQMember.AddIcon(Ico: TCnQQIco);
var
  len: Integer;
begin
  len := Length(FQQIcos);
{$IFDEF DELPHI2007_UP}
  ico.Image.OnMouseEnter := OnImageMouseEnter;
  ico.Image.OnMouseLeave := OnImageMouseLeave;
{$ENDIF}
  SetLength(FQQIcos, len + 1);
  FQQIcos[len] := ico;

  FQQIcos[len].Parent := FPExtension;
  FQQIcos[len].Align := alLeft;
end;

constructor TCnQQMember.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Height := 54;
  Color := clWindow;
{$IFDEF DELPHI2007_UP}
  OnMouseEnter := OnGlassMouseEnter;
  OnMouseLeave := OnGlassMouseLeave;
{$ENDIF}
  OnClick := OnGlassClick;
  OnDblClick := OnGlassDoubleClick;

  FHeadImage := TImage.Create(self);
  FHeadImage.Parent := self;
{$IFDEF DELPHI2007_UP}
  FHeadImage.Margins.Top := 7;
  FHeadImage.Margins.Bottom := 7;
  FHeadImage.Margins.Right := 15;
  FHeadImage.AlignWithMargins := True;
{$ENDIF}
  FHeadImage.Align := alLeft;
  FHeadImage.Height := 40;
  FHeadImage.Width := 40;
  FHeadImage.Transparent := True;

  FPnlCont := TPanel.Create(self);
  FPnlCont.Parent := self;
  FPnlCont.BevelOuter := bvNone;
  FPnlCont.Caption := EmptyStr;
  FPnlCont.Align := alClient;
  FPnlCont.Color := clWindow;

{$IFDEF DELPHI2007_UP}
  FPnlCont.OnMouseEnter := OnGlassMouseEnter;
  FPnlCont.OnMouseLeave := OnGlassMouseLeave;
{$ENDIF}  
  FPnlCont.OnClick := OnGlassClick;
  FPnlCont.OnDblClick := OnGlassDoubleClick;

  FPNickName:= TPanel.Create(Self);
  FPNickName.Parent := FPnlCont;
  FPNickName.BevelOuter := bvNone;
  FPNickName.Caption := EmptyStr;
  FPNickName.Height := 16;
  FPNickName.Align := alTop;
  FPNickName.Color := clWindow;

  FPDesc:= TPanel.Create(Self);
  FPDesc.Parent := FPnlCont;
  FPDesc.BevelOuter := bvNone;
  FPDesc.Caption := EmptyStr;
  FPDesc.Height := 16;
  FPDesc.Align := alTop;
  FPDesc.Color := clWindow;
  FPDesc.Font.Color := clGray;

  FPExtension:= TPanel.Create(Self);
  FPExtension.Parent := FPnlCont;
  FPExtension.BevelOuter := bvNone;
  FPExtension.Caption := EmptyStr;
  FPExtension.Height := 18;
  FPExtension.Align := alClient;
  FPExtension.Color := clWindow;

{$IFDEF DELPHI2007_UP}
  FHeadImage.OnMouseEnter := OnGlassMouseEnter;
  FHeadImage.OnMouseLeave := OnGlassMouseLeave;
{$ENDIF}

  FHeadImage.OnClick := OnGlassClick;
  FHeadImage.OnDblClick := OnGlassDoubleClick;

  FNickName := TLabel.Create(self);
  FNickName.Parent := FPNickName;
  FNickName.Color := clWindow;
  FNickName.Align := alClient;
  FNickName.Layout := tlCenter;
  
  FDesc := TLabel.Create(self);
  FDesc.Parent := FPDesc;
  FDesc.Color := clWindow;
  FDesc.Align := alClient;
  FDesc.Layout := tlCenter;

  FExtension := TLabel.Create(self);
  FExtension.Parent := FPExtension;
  FExtension.Color := clWindow;
  FExtension.Align := alClient;
  FExtension.Layout := tlCenter;

{$IFDEF DELPHI2007_UP}
  FNickName.OnMouseEnter := OnGlassMouseEnter;
  FNickName.OnMouseLeave := OnGlassMouseLeave;
{$ENDIF}

  FNickName.OnClick := OnGlassClick;
  FNickName.OnDblClick := OnGlassDoubleClick;

{$IFDEF DELPHI2007_UP}
  FDesc.OnMouseEnter := OnGlassMouseEnter;
  FDesc.OnMouseLeave := OnGlassMouseLeave;
{$ENDIF}
  FDesc.OnClick := OnGlassClick;
  FDesc.OnDblClick := OnGlassDoubleClick;

{$IFDEF DELPHI2007_UP}
  FExtension.OnMouseEnter := OnGlassMouseEnter;
  FExtension.OnMouseLeave := OnGlassMouseLeave;
{$ENDIF}
  FExtension.OnClick := OnGlassClick;
  FExtension.OnDblClick := OnGlassDoubleClick;
end;

destructor TCnQQMember.Destroy;
begin

  inherited;
end;

procedure TCnQQMember.OnGlassClick(Sender: TObject);
begin
  if Parent.Parent <> nil then
  begin
    if Parent.Parent.Parent <> nil then
    begin
      if Parent.Parent.Parent.ClassName = 'TCnQQPanel' then
      begin
        if Assigned(TCnQQPanel(Parent.Parent.Parent).OnMemberClick) then
          TCnQQPanel(Parent.Parent.Parent).OnMemberClick(self, Data);
      end;
    end;
  end;
end;

procedure TCnQQMember.OnGlassDoubleClick(Sender: TObject);
begin
  if Parent.Parent <> nil then
  begin
    if Parent.Parent.Parent <> nil then
    begin
      if Parent.Parent.Parent.ClassName = 'TCnQQPanel' then
      begin
        if Assigned(TCnQQPanel(Parent.Parent.Parent).OnMemberDblClick) then
          TCnQQPanel(Parent.Parent.Parent).OnMemberDblClick(self, Data);
      end;
    end;
  end;
end;

procedure TCnQQMember.OnGlassMouseEnter(Sender: TObject);
begin
  Color := $00F9F0EA;
  FPnlCont.Color := $00F9F0EA;
  FPNickName.Color := $00F9F0EA;
  FPDesc.Color := $00F9F0EA;
  FPExtension.Color := $00F9F0EA;
end;

procedure TCnQQMember.OnGlassMouseLeave(Sender: TObject);
begin
  Color := clWindow;
  FPnlCont.Color := clWindow;
  FPNickName.Color := clWindow;
  FPDesc.Color := clWindow;
  FPExtension.Color := clWindow;
end;

procedure TCnQQMember.OnImageMouseEnter(Sender: TObject);
begin
  //
  OnGlassMouseEnter(Self);
  if FileExists(TCnQQIco(TImage(Sender).Parent).HotIcon) then
    TImage(Sender).Picture.Bitmap.LoadFromFile(
      TCnQQIco(TImage(Sender).Parent).HotIcon);
  // TPanel(TImage(Sender).Parent).Color := $00C2804B;
end;

procedure TCnQQMember.OnImageMouseLeave(Sender: TObject);
begin
  //
  OnGlassMouseLeave(Self);
  TImage(Sender).Picture.Bitmap.LoadFromFile(
    TCnQQIco(TImage(Sender).Parent).NormalIcon);
 // TPanel(TImage(Sender).Parent).ParentColor := True;
end;

procedure TCnQQMember.RemoveIcon(Index: Integer);
var
  len: Integer;
  i: integer;
begin
  len := Length(FQQIcos);
  FQQIcos[Index].Free;
  for i := Index to len - 2 do
  begin
    FQQIcos[i] := FQQIcos[i+1];
  end;
  SetLength(FQQIcos, len - 1);
end;

{ TCnQQGroup }

function TCnQQGroup.AddMember(person: TCnQQPerson): Boolean;
var
  i: integer;
  userAdded: boolean;
  len: integer;
  j: integer;
begin
  userAdded := False;
  for i := 0 to Length(FMembers) - 1 do
  begin
    if FMembers[i].UserID = person.UserID then
    begin
      userAdded := True;
      Break;
    end;
  end;
  if userAdded then
  begin
    Result := False;
    Exit;
  end;
  len := Length(FMembers);
  SetLength(FMembers, len + 1);
  FMembers[len] := TCnQQMember.Create(self);
  FMembers[len].Parent := self;
  FMembers[len].Caption := EmptyStr;
  FMembers[len].Align := alTop;
  if len = 0 then
    FMembers[len].Top := 0
  else
    FMembers[len].Top := FMembers[len-1].Top + FMembers[len-1].Height + 1;
  FMembers[len].NickName.Caption := person.UserName;
  FMembers[len].NickName.Font.Color := person.NameColor;
  FMembers[len].Desc.Caption := person.UserDesc;
  FMembers[len].UserID := person.UserID;
  if FileExists(person.UserHead) then
    FMembers[len].HeadImage.Picture.LoadFromFile(person.UserHead);
  for j := 0 to Length(person.FUserIcons) - 1 do
    FMembers[len].AddIcon(person.FUserIcons[j]);
  FMembers[len].Data := person;

  Height := 54 * (len + 1);
  if Owner.ClassName = 'TCnQQGroupWTitle' then
    TCnQQGroupWTitle(Owner).Height := 22 + Height;
  Result := True;
end;

constructor TCnQQGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color := clWindow;
  SetMemberCount(0);
end;

function TCnQQGroup.GetMemberCount: Integer;
begin
  Result := Length(FMembers);
end;

procedure TCnQQGroup.RemoveMember(Index: Integer);
var
  i: integer;
  len: integer;
begin
  FMembers[Index].Free;
  for i := Index to Length(FMembers) - 2 do
  begin
    FMembers[i] := FMembers[i+1];
  end;

  len := Length(FMembers);
  SetLength(FMembers, len - 1);

  Height := 54 * (len - 1);
  if Owner.ClassName = 'TCnQQGroupWTitle' then
    TCnQQGroupWTitle(Owner).Height := 22 + Height;
end;

procedure TCnQQGroup.SetMemberCount(const Value: Integer);
var
  i: integer;
begin
  for i := Length(FMembers) - 1 downto 0 do
    FMembers[i].Free;

  SetLength(FMembers, Value);
  for i := 0 to Length(FMembers) - 1 do
  begin
    FMembers[i] := TCnQQMember.Create(self);
    FMembers[i].Parent := self;
    FMembers[i].Caption := EmptyStr;
    FMembers[i].Align := alTop;
    if i = 0 then
      FMembers[i].Top := 0
    else
      FMembers[i].Top := FMembers[i-1].Top + FMembers[i-1].Height + 1;
  end;
  Height := 54 * Value;
  if Owner.ClassName = 'TCnQQGroupWTitle' then
    TCnQQGroupWTitle(Owner).Height := 22 + Height;
end;

{ TCnQQGroupWTitle }

constructor TCnQQGroupWTitle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color := clWindow;
  
  FTitle := TPanel.Create(self);
  FTitle.Parent := self;
  FTitle.BevelOuter := bvNone;
  FTitle.Height := 22;
  FTitle.Align := alTop;
  FTitle.Caption := EmptyStr;
  FTitle.OnClick := OnTitleClick;
  FTitle.Color := clWindow;
{$IFDEF DELPHI2007_UP}
  FTitle.OnMouseEnter := OnTitleMouseEnter;
  FTitle.OnMouseLeave := OnTitleMouseLeave;
{$ENDIF}
  FTitleImage:= TImage.Create(Self);
  FTitleImage.Parent := FTitle;
{$IFDEF DELPHI2007_UP}
  FTitleImage.AlignWithMargins := True;
{$ENDIF}
  FTitleImage.Align := alLeft;
  FTitleImage.Height := 16;
  FTitleImage.Width := 16;
  FTitleImage.OnClick := OnTitleClick;
  FTitleImage.Transparent := True;
{$IFDEF DELPHI2007_UP}
  FTitleImage.OnMouseEnter := OnTitleMouseEnter;
  FTitleImage.OnMouseLeave := OnTitleMouseLeave;
{$ENDIF}
  FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROW');

  FTitleName := TLabel.Create(self);
  FTitleName.Parent := FTitle;
{$IFDEF DELPHI2007_UP}
  FTitleName.AlignWithMargins := True;
{$ENDIF}
  FTitleName.Align := alClient;
  FTitleName.Caption := EmptyStr;
  FTitleName.Layout := tlCenter;
  FTitleName.OnClick := OnTitleClick;
  FTitleName.Color := clWindow;
  FTitleName.OnMouseEnter := OnTitleMouseEnter;
  FTitleName.OnMouseLeave := OnTitleMouseLeave;

  FGroup:= TCnQQGroup.Create(Self);
  FGroup.Parent := self;
  FGroup.Caption := EmptyStr;
  FGroup.Top := 23;
  FGroup.Align := alTop;
  FGroup.Color := clWindow;

end;

procedure TCnQQGroupWTitle.Expand;
begin
  if not FGroup.Visible then
  begin
    FGroup.Show;
    FGroup.Top := 23;
    FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROWDOWN');
    Height := 22 + FGroup.Height;
    // notify
    if Owner.ClassName = 'TCnQQPanel'  then
      TCnQQPanel(Owner).SetPanelHeight;
  end;
end;

procedure TCnQQGroupWTitle.OnTitleClick(SendeR: TObject);
begin
  if FGroup.Visible then
  begin
    FGroup.Hide;
    FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROW');
    Height := 22;
    // notify
    if Owner.ClassName = 'TCnQQPanel'  then
      TCnQQPanel(Owner).SetPanelHeight;
  end
  else
  begin

    FGroup.Top := 23;
    FGroup.Show;
    FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROWDOWN');
    Height := 22 + FGroup.Height;
    // notify
    if Owner.ClassName = 'TCnQQPanel'  then
      TCnQQPanel(Owner).SetPanelHeight;
  end;  
end;

procedure TCnQQGroupWTitle.OnTitleMouseEnter(Sender: TObject);
begin
  FTitle.Color := $00F8ECE4;
end;

procedure TCnQQGroupWTitle.OnTitleMouseLeave(Sender: TObject);
begin
  FTitle.Color := clWindow;
end;

procedure TCnQQGroupWTitle.Packup;
begin
  if FGroup.Visible then
  begin
    FGroup.Hide;
    FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROW');
    Height := 22;
    // notify
    if Owner.ClassName = 'TCnQQPanel'  then
      TCnQQPanel(Owner).SetPanelHeight;
  end;
  
end;

{ TCnQQPanel }

function TCnQQPanel.AddGroup(AName: string): boolean;
var
  len: integer;
  i: integer;
  hasGroup: boolean;
begin
  hasGroup := False;
  len := length(FGroups);
  for i := 0 to len - 1 do
  begin
    if FGroups[i].FTitleName.Caption = AName then
    begin
      hasGroup := True;
      Break;
    end;
  end;
  if hasGroup then
  begin
    Result := False;
    Exit;
  end;
  SetLength(FGroups, len + 1);
  FGroups[len] := TCnQQGroupWTitle.Create(self);
  FGroups[len].Parent := self;
  FGroups[len].Caption := EmptyStr;
  FGroups[len].Align := alTop;
  FGroups[len].FTitleName.Caption := AName;
  if len = 0 then
    FGroups[len].Top := 0
  else
    FGroups[len].Top := FGroups[len-1].Top + FGroups[len-1].Height + 1;
  SetPanelHeight;
  Result := True;
end;

constructor TCnQQPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color := clWindow;
end;

procedure TCnQQPanel.ExpandAll;
var
  i: integer;
begin
  for i := 0 to length(FGroups) - 1 do
    FGroups[i].Expand;
end;

function TCnQQPanel.GetGroupCount: Integer;
begin
  Result := Length(FGroups);
end;

procedure TCnQQPanel.PackupAll;
var
  i: integer;
begin
  for i := 0 to length(FGroups) - 1 do
    FGroups[i].Packup;

end;

procedure TCnQQPanel.RemoveGroup(Index: Integer);
var
  len: integer;
  i: integer;
begin
  len := length(FGroups);
  FGroups[Index].Free;
  for i := Index to len - 2 do
    FGroups[i] := FGroups[i+1];
  SetLength(FGroups, len - 1 );
  SetPanelHeight;
end;

procedure TCnQQPanel.SetGroupCount(const Value: Integer);
var
  i: integer;
begin
  SetLength(FGroups, Value);
  for i := 0 to Length(FGroups) - 1 do
  begin
    if not Assigned(FGroups[i]) then
    begin
      FGroups[i] := TCnQQGroupWTitle.Create(self);
      FGroups[i].Parent := self;
      FGroups[i].Caption := EmptyStr;
      FGroups[i].Align := alTop;
      if i = 0 then
        FGroups[i].Top := 0
      else
        FGroups[i].Top := FGroups[i-1].Top + FGroups[i-1].Height + 1;
    end;
  end;
  SetPanelHeight;
end;

procedure TCnQQPanel.SetPanelHeight;
var
  i: integer;
  h: integer;
begin
  //

  h := 0;
  for i := 0 to Length(FGroups) - 1 do
    h := h + FGroups[i].Height;
  Height := h;
end;

{ TCnQQIco }

constructor TCnQQIco.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
  ParentColor := True;
{$IFDEF DELPHI2007_UP}
  AlignWithMargins := True;
  Margins.Right :=3;
  Margins.Top := 2;
  Margins.Bottom := 2;
  Margins.Left := 2;
{$ENDIF}
  Height := 18;
  Width := 18;

  FImage := TImage.Create(self);
  FImage.Parent := self;
  FImage.Stretch := True;
  FImage.Align := alClient;
  FImage.Transparent := True;

  FImage.OnClick := OnImgClick;
  FImage.OnDblClick := OnImgDblClick;
end;


procedure TCnQQIco.OnImgClick(Sender: TObject);
begin
  if Assigned(OnIconClick) then
    OnIconClick(Self, Data);
end;

procedure TCnQQIco.OnImgDblClick(Sender: TObject);
begin
  if Assigned(OnIconDoubleClick) then
    OnIconDoubleClick(Self, Data);
end;

procedure TCnQQIco.SetData(const Value: Pointer);
begin
  FData := Value;
  FImage.ShowHint := FData <> nil;
  if FData <> nil then
  begin
    FImage.Hint := TCnQQIconData(Value).IconDesc;
  end;
end;

procedure TCnQQIco.SetNormalIcon(const Value: string);
begin
  FNormalIcon := Value;
  FImage.Picture.Bitmap.LoadFromFile(Value);
end;

{ TCnQQPerson }

procedure TCnQQPerson.AddIcon(Ico: TCnQQIco);
var
  len: integer;
begin
  len:=length(FUserIcons);
  SetLength(FUserIcons, len + 1);
  FUserIcons[len] := ico;
end;

constructor TCnQQPerson.Create;
begin
  FNameColor := clBlack;
end;

procedure TCnQQPerson.RemoveIcon(Index: Integer);
var
  i: integer;
  len: integer;
begin
  len := length(FUserIcons);
  FUserIcons[Index].Free;
  for i := index to len - 2 do
    FUserIcons[i] := FUserIcons[i+1];
  Setlength(FUserIcons, len -1);
end;

end.
