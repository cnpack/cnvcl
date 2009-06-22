unit CnFMQQPane;

interface

uses
  Windows, Classes, SysUtils, StdCtrls, Graphics, ExtCtrls, CnFMForm, Controls,
  Forms;

type
  TCnFMMemberNotifyEvent = procedure(Sender: TObject; AData: Pointer) of object;
  TCnFMIconNotifyEvent = procedure(Sender: TObject; AData: Pointer) of object;

  TCnFMQQIcoData = class
  private
    FIcoDesc: string;
    FIcoName: string;
    FIcoID: string;
  public
    property IcoID: string read FIcoID write FIcoID;
    property IcoName: string read FIcoName write FIcoName;
    property IcoDesc: string read FIcoDesc write FIcoDesc;
  end;
  
  TCnFMQQIco = class(TPanel)
  private
    FImage: TImage;
    FData: Pointer;
    FOnIconClick: TCnFMIconNotifyEvent;
    FOnIconDoubleClick: TCnFMIconNotifyEvent;
    FNormalIco: string;
    FHotIco: string;
    procedure SetNormalIco(const Value: string);
    procedure SetData(const Value: Pointer);
  protected
    procedure OnImgClick(Sender: TObject);
    procedure OnImgDblClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  public
    property Data: Pointer read FData write SetData;
    property Image: TImage read  FImage write FImage;
    property NormalIco: string read FNormalIco write SetNormalIco;
    property HotIco: string read FHotIco write FHotIco;
    property OnIconClick: TCnFMIconNotifyEvent read FOnIconClick write FOnIconClick;
    property OnIconDoubleClick: TCnFMIconNotifyEvent read FOnIconDoubleClick write FOnIconDoubleClick;
  end;

  TCnFMQQIcoArray = array of TCnFMQQIco;

  TCnFMQQPerson = class
  private
    FUserID: string;
    FUserDesc: string;
    FUserName: string;
    FUserIcons: TCnFMQQIcoArray;
    FUserHead: string;
    FNameColor: TColor;
  public
    procedure AddIcon(Ico: TCnFMQQIco);
    procedure RemoveIcon(Index: Integer);
    constructor Create;
  public
    property UserID: string read FUserID write FUserID;
    property UserName: string read FUserName write FUserName;
    property UserDesc: string read FUserDesc write FUserDesc;
    property UserIcons: TCnFMQQIcoArray read FUserIcons write FUserIcons;
    property UserHead: string read FUserHead write FUserHead;
    property NameColor: TColor read FNameColor write FNameColor default clBlack;
  end;

  TCnFMQQMember = class(TPanel)
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
    FQQIcos: TCnFMQQIcoArray;
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
    procedure AddIcon(Ico: TCnFMQQIco);
    procedure RemoveIcon(Index: Integer);
  public
    property Data: Pointer read FData write FData;
    property QQIcons: TCnFMQQIcoArray read  FQQIcos write FQQIcos;
  published
    property UserID: string read FUserID write FUserID;
    property HeadImage: TImage read FHeadImage write FHeadImage;
    property NickName: TLabel read FNickName write FNickName;
    property Desc: TLabel read FDesc write FDesc;
    property Extension: TLabel read FExtension write FExtension;
  end;

  TCnFMQQMemberArray = array of TCnFMQQMember;

  TCnFMQQGroup = class(TPanel)
  private
    FMembers: TCnFMQQMemberArray;
    function GetMemberCount: Integer;
    procedure SetMemberCount(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function AddMember(person: TCnFMQQPerson): Boolean;
    procedure RemoveMember(Index: Integer);
  public
    property Members: TCnFMQQMemberArray read FMembers write FMembers;
  published
    property MemberCount: Integer read GetMemberCount write SetMemberCount;
  end;

  TCnFMQQGroupWTitle = class(TPanel)
  private
    FTitle: TPanel;
    FGroup: TCnFMQQGroup;
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
    property QQGroup: TCnFMQQGroup read FGroup write FGroup;
  end;

  TCnFMQQGroupWTitleArray = array of TCnFMQQGroupWTitle;

  TCnFMQQPanel = class(TScrollBox)
  private
    FGroups: TCnFMQQGroupWTitleArray;
    FOnMemberClick: TCnFMMemberNotifyEvent;
    FOnMemberDblClick: TCnFMMemberNotifyEvent;
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
    property Groups: TCnFMQQGroupWTitleArray read FGroups write FGroups;
  published
    property GroupCount: Integer read GetGroupCount write SetGroupCount;
    property OnMemberClick: TCnFMMemberNotifyEvent read FOnMemberClick write FOnMemberClick;
    property OnMemberDblClick: TCnFMMemberNotifyEvent read FOnMemberDblClick write FOnMemberDblClick;
  end;

implementation

{$R Arrows.RES}

{ TCnFMQQMember }

procedure TCnFMQQMember.AddIcon(Ico: TCnFMQQIco);
var
  len: Integer;
begin
  len := Length(FQQIcos);
  ico.Image.OnMouseEnter := OnImageMouseEnter;
  ico.Image.OnMouseLeave := OnImageMouseLeave;
  SetLength(FQQIcos, len + 1);
  FQQIcos[len] := ico;

  FQQIcos[len].Parent := FPExtension;
  FQQIcos[len].Align := alLeft;
end;

constructor TCnFMQQMember.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Height := 54;
  Color := clWindow;

  OnMouseEnter := OnGlassMouseEnter;
  OnMouseLeave := OnGlassMouseLeave;
  OnClick := OnGlassClick;
  OnDblClick := OnGlassDoubleClick;

  FHeadImage := TImage.Create(self);
  FHeadImage.Parent := self;
  FHeadImage.Margins.Top := 7;
  FHeadImage.Margins.Bottom := 7;
  FHeadImage.Margins.Right := 15;
  FHeadImage.AlignWithMargins := True;
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

  FPnlCont.OnMouseEnter := OnGlassMouseEnter;
  FPnlCont.OnMouseLeave := OnGlassMouseLeave;
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

//  FGlass:= TRaNGlassPanel.Create(Self);
//  FGlass.Parent := self;
//  FGlass.Left := 0;
//  FGlass.Top := 0;
//  FGlass.Width := self.Width;
//  FGlass.Height := self.Height;
//  FGlass.BringToFront;
//
//  FGlass.OnMouseEnter := OnGlassMouseEnter;
//  FGlass.OnMouseLeave := OnGlassMouseLeave;
//  FGlass.OnClick := OnGlassClick;
//  FGlass.OnDblClick := OnGlassDoubleClick;

  FHeadImage.OnMouseEnter := OnGlassMouseEnter;
  FHeadImage.OnMouseLeave := OnGlassMouseLeave;
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

  FNickName.OnMouseEnter := OnGlassMouseEnter;
  FNickName.OnMouseLeave := OnGlassMouseLeave;
  FNickName.OnClick := OnGlassClick;
  FNickName.OnDblClick := OnGlassDoubleClick;

  FDesc.OnMouseEnter := OnGlassMouseEnter;
  FDesc.OnMouseLeave := OnGlassMouseLeave;
  FDesc.OnClick := OnGlassClick;
  FDesc.OnDblClick := OnGlassDoubleClick;

  FExtension.OnMouseEnter := OnGlassMouseEnter;
  FExtension.OnMouseLeave := OnGlassMouseLeave;
  FExtension.OnClick := OnGlassClick;
  FExtension.OnDblClick := OnGlassDoubleClick;

end;

destructor TCnFMQQMember.Destroy;
begin

  inherited;
end;

procedure TCnFMQQMember.OnGlassClick(Sender: TObject);
begin
  if Parent.Parent <> nil then
  begin
    if Parent.Parent.Parent <> nil then
    begin
      if Parent.Parent.Parent.ClassName = 'TCnFMQQPanel' then
      begin
        if Assigned(TCnFMQQPanel(Parent.Parent.Parent).OnMemberClick) then
          TCnFMQQPanel(Parent.Parent.Parent).OnMemberClick(self, Data);
      end;
    end;
  end;
end;

procedure TCnFMQQMember.OnGlassDoubleClick(Sender: TObject);
begin
  if Parent.Parent <> nil then
  begin
    if Parent.Parent.Parent <> nil then
    begin
      if Parent.Parent.Parent.ClassName = 'TCnFMQQPanel' then
      begin
        if Assigned(TCnFMQQPanel(Parent.Parent.Parent).OnMemberDblClick) then
          TCnFMQQPanel(Parent.Parent.Parent).OnMemberDblClick(self, Data);
      end;
    end;
  end;
end;

procedure TCnFMQQMember.OnGlassMouseEnter(Sender: TObject);
begin
  Color := $00F9F0EA;
  FPnlCont.Color := $00F9F0EA;
  FPNickName.Color := $00F9F0EA;
  FPDesc.Color := $00F9F0EA;
  FPExtension.Color := $00F9F0EA;
end;

procedure TCnFMQQMember.OnGlassMouseLeave(Sender: TObject);
begin
  Color := clWindow;
  FPnlCont.Color := clWindow;
  FPNickName.Color := clWindow;
  FPDesc.Color := clWindow;
  FPExtension.Color := clWindow;
end;

procedure TCnFMQQMember.OnImageMouseEnter(Sender: TObject);
begin
  //
  OnGlassMouseEnter(Self);
  if FileExists(TCnFMQQIco(TImage(Sender).Parent).HotIco) then
    TImage(Sender).Picture.Bitmap.LoadFromFile(
      TCnFMQQIco(TImage(Sender).Parent).HotIco);
  // TPanel(TImage(Sender).Parent).Color := $00C2804B;
end;

procedure TCnFMQQMember.OnImageMouseLeave(Sender: TObject);
begin
  //
  OnGlassMouseLeave(Self);
  TImage(Sender).Picture.Bitmap.LoadFromFile(
    TCnFMQQIco(TImage(Sender).Parent).NormalIco);
 // TPanel(TImage(Sender).Parent).ParentColor := True;
end;

procedure TCnFMQQMember.RemoveIcon(Index: Integer);
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

{ TCnFMQQGroup }

function TCnFMQQGroup.AddMember(person: TCnFMQQPerson): Boolean;
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
  FMembers[len] := TCnFMQQMember.Create(self);
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
  if Owner.ClassName = 'TCnFMQQGroupWTitle' then
    TCnFMQQGroupWTitle(Owner).Height := 22 + Height;
  Result := True;
end;

constructor TCnFMQQGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color := clWindow;
  SetMemberCount(0);
end;

function TCnFMQQGroup.GetMemberCount: Integer;
begin
  Result := Length(FMembers);
end;

procedure TCnFMQQGroup.RemoveMember(Index: Integer);
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
  if Owner.ClassName = 'TCnFMQQGroupWTitle' then
    TCnFMQQGroupWTitle(Owner).Height := 22 + Height;
end;

procedure TCnFMQQGroup.SetMemberCount(const Value: Integer);
var
  i: integer;
begin
  for i := Length(FMembers) - 1 downto 0 do
    FMembers[i].Free;

  SetLength(FMembers, Value);
  for i := 0 to Length(FMembers) - 1 do
  begin
    FMembers[i] := TCnFMQQMember.Create(self);
    FMembers[i].Parent := self;
    FMembers[i].Caption := EmptyStr;
    FMembers[i].Align := alTop;
    if i = 0 then
      FMembers[i].Top := 0
    else
      FMembers[i].Top := FMembers[i-1].Top + FMembers[i-1].Height + 1;
  end;
  Height := 54 * Value;
  if Owner.ClassName = 'TCnFMQQGroupWTitle' then
    TCnFMQQGroupWTitle(Owner).Height := 22 + Height;
end;

{ TCnFMQQGroupWTitle }

constructor TCnFMQQGroupWTitle.Create(AOwner: TComponent);
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
  FTitle.OnMouseEnter := OnTitleMouseEnter;
  FTitle.OnMouseLeave := OnTitleMouseLeave;

  FTitleImage:= TImage.Create(Self);
  FTitleImage.Parent := FTitle;
  FTitleImage.AlignWithMargins := True;
  FTitleImage.Align := alLeft;
  FTitleImage.Height := 16;
  FTitleImage.Width := 16;
  FTitleImage.OnClick := OnTitleClick;
  FTitleImage.Transparent := True;
  FTitleImage.OnMouseEnter := OnTitleMouseEnter;
  FTitleImage.OnMouseLeave := OnTitleMouseLeave;
  FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROW');

  FTitleName := TLabel.Create(self);
  FTitleName.Parent := FTitle;
  FTitleName.AlignWithMargins := True;
  FTitleName.Align := alClient;
  FTitleName.Caption := EmptyStr;
  FTitleName.Layout := tlCenter;
  FTitleName.OnClick := OnTitleClick;
  FTitleName.Color := clWindow;
  FTitleName.OnMouseEnter := OnTitleMouseEnter;
  FTitleName.OnMouseLeave := OnTitleMouseLeave;

  FGroup:= TCnFMQQGroup.Create(Self);
  FGroup.Parent := self;
  FGroup.Caption := EmptyStr;
  FGroup.Top := 23;
  FGroup.Align := alTop;
  FGroup.Color := clWindow;

end;

procedure TCnFMQQGroupWTitle.Expand;
begin
  if not FGroup.Visible then
  begin
    FGroup.Show;
    FGroup.Top := 23;
    FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROWDOWN');
    Height := 22 + FGroup.Height;
    // notify
    if Owner.ClassName = 'TCnFMQQPanel'  then
      TCnFMQQPanel(Owner).SetPanelHeight;
  end;
  
end;

procedure TCnFMQQGroupWTitle.OnTitleClick(SendeR: TObject);
begin
  if FGroup.Visible then
  begin
    FGroup.Hide;
    FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROW');
    Height := 22;
    // notify
    if Owner.ClassName = 'TCnFMQQPanel'  then
      TCnFMQQPanel(Owner).SetPanelHeight;
  end
  else
  begin

    FGroup.Top := 23;
    FGroup.Show;
    FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROWDOWN');
    Height := 22 + FGroup.Height;
    // notify
    if Owner.ClassName = 'TCnFMQQPanel'  then
      TCnFMQQPanel(Owner).SetPanelHeight;
  end;  
end;

procedure TCnFMQQGroupWTitle.OnTitleMouseEnter(Sender: TObject);
begin
  FTitle.Color := $00F8ECE4;
end;

procedure TCnFMQQGroupWTitle.OnTitleMouseLeave(Sender: TObject);
begin
  FTitle.Color := clWindow;
end;

procedure TCnFMQQGroupWTitle.Packup;
begin
  if FGroup.Visible then
  begin
    FGroup.Hide;
    FTitleImage.Picture.Bitmap.LoadFromResourceName(HInstance,'ARROW');
    Height := 22;
    // notify
    if Owner.ClassName = 'TCnFMQQPanel'  then
      TCnFMQQPanel(Owner).SetPanelHeight;
  end;
  
end;

{ TCnFMQQPanel }

function TCnFMQQPanel.AddGroup(AName: string): boolean;
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
  FGroups[len] := TCnFMQQGroupWTitle.Create(self);
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

constructor TCnFMQQPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color := clWindow;
end;

procedure TCnFMQQPanel.ExpandAll;
var
  i: integer;
begin
  for i := 0 to length(FGroups) - 1 do
    FGroups[i].Expand;
end;

function TCnFMQQPanel.GetGroupCount: Integer;
begin
  Result := Length(FGroups);
end;

procedure TCnFMQQPanel.PackupAll;
var
  i: integer;
begin
  for i := 0 to length(FGroups) - 1 do
    FGroups[i].Packup;

end;

procedure TCnFMQQPanel.RemoveGroup(Index: Integer);
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

procedure TCnFMQQPanel.SetGroupCount(const Value: Integer);
var
  i: integer;
begin
  SetLength(FGroups, Value);
  for i := 0 to Length(FGroups) - 1 do
  begin
    if not Assigned(FGroups[i]) then
    begin
      FGroups[i] := TCnFMQQGroupWTitle.Create(self);
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

procedure TCnFMQQPanel.SetPanelHeight;
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

{ TCnFMQQIco }

constructor TCnFMQQIco.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
  ParentColor := True;
  
  AlignWithMargins := True;
  Margins.Right :=3;
  Margins.Top := 2;
  Margins.Bottom := 2;
  Margins.Left := 2;

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


procedure TCnFMQQIco.OnImgClick(Sender: TObject);
begin
  if Assigned(OnIconClick) then
    OnIconClick(Self, Data);
end;

procedure TCnFMQQIco.OnImgDblClick(Sender: TObject);
begin
  if Assigned(OnIconDoubleClick) then
    OnIconDoubleClick(Self, Data);
end;

procedure TCnFMQQIco.SetData(const Value: Pointer);
begin
  FData := Value;
  FImage.ShowHint := FData <> nil;
  if FData <> nil then
  begin
    FImage.Hint := TCnFMQQIcoData(Value).IcoDesc;
  end;
end;

procedure TCnFMQQIco.SetNormalIco(const Value: string);
begin
  FNormalIco := Value;
  FImage.Picture.Bitmap.LoadFromFile(Value);
end;

{ TCnFMQQPerson }

procedure TCnFMQQPerson.AddIcon(Ico: TCnFMQQIco);
var
  len: integer;
begin
  len:=length(FUserIcons);
  SetLength(FUserIcons, len + 1);
  FUserIcons[len] := ico;
end;

constructor TCnFMQQPerson.Create;
begin
  FNameColor := clBlack;
end;

procedure TCnFMQQPerson.RemoveIcon(Index: Integer);
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
