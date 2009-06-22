unit CnFMAppStore;

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Graphics, CnFMButton;

type
  TCnFMAppStatus = (asNotInstalled, asInstalled, asCanUpdate);

  TCnFMAppStoreItemClick = procedure(Sender: TObject; AData: Pointer) of object;

  TCnFMAppStore = class(TPanel)
  private
    FImage: TImage;
    FRight: TPanel;
    FCenter: TPanel;
    FLblSize: TLabel;
    FImgInstall: TImage;
    FBtnDown: TCnFMButton;
    FStatus: TCnFMAppStatus;
    FlblName: TLabel;
    FlblDesc: TLabel;
    FOnItemDblClick: TCnFMAppStoreItemClick;
    FOnItemClick: TCnFMAppStoreItemClick;
    FData: Pointer;
    FOnButtonClick: TCnFMAppStoreItemClick;
    function GetAppSize: string;
    procedure SetAppSize(const Value: string);
    procedure SetStatus(const Value: TCnFMAppStatus);
    function GetAppDesc: string;
    function GetAppName: string;
    procedure SetAppDesc(const Value: string);
    procedure SetAppName(const Value: string);
  protected
    procedure FOnAppStoreMouseEnter(Sender: TObject);
    procedure FOnAppStoreMouseLeave(Sender: TObject);
    procedure FOnAppStoreMouseClick(Sender: TObject);
    procedure FOnAppStoreMouseDblClick(Sender: TObject);
    procedure FOnAppStoreButtonClick(Sender: TObject);
  public
    procedure CreateWnd; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Data: Pointer read FData write FData;
  published
    property Image: TImage read FImage write FImage;
    property AppSize: string read GetAppSize write SetAppSize;
    property Status:TCnFMAppStatus read FStatus write SetStatus;
    property AppName: string read GetAppName write SetAppName;
    property AppDesc: string read GetAppDesc write SetAppDesc;
    property OnItemClick: TCnFMAppStoreItemClick read FOnItemClick write FOnItemClick;
    property OnItemDblClick: TCnFMAppStoreItemClick read FOnItemDblClick write FOnItemDblClick;
    property OnButtonClick: TCnFMAppStoreItemClick read FOnButtonClick write FOnButtonClick;
  end;

implementation

{$R AppStore.RES}

{ TCnFMAppStore }

constructor TCnFMAppStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color := clWhite;
  Height := 46;
  Width := 400;

  FImage := TImage.Create(self);
  FImage.Parent := self;
  FImage.Height := 32;
  FImage.Width := 32;
  FImage.AlignWithMargins := True;
  FImage.Margins.Top := 7;
  FImage.Margins.Bottom := 7;
  FImage.Margins.Left := 7;
  FImage.Margins.Right := 7;
  FImage.Align := alLeft;
  FImage.OnMouseEnter := FOnAppStoreMouseEnter;
  FImage.OnMouseLeave := FOnAppStoreMouseLeave;
  FImage.OnClick := FOnAppStoreMouseClick;
  FImage.OnDblClick := FOnAppStoreMouseDblClick;

  FRight:= TPanel.Create(self);
  FRight.Parent := self;
  FRight.BevelOuter := bvNone;
  FRight.Width := 242;
  FRight.Align := alRight;
  FRight.ParentColor := True;
  FRight.OnMouseEnter := FOnAppStoreMouseEnter;
  FRight.OnMouseLeave := FOnAppStoreMouseLeave;
  FRight.OnClick := FOnAppStoreMouseClick;
  FRight.OnDblClick := FOnAppStoreMouseDblClick;

  FCenter:= TPanel.Create(self);
  FCenter.Parent := self;
  FCenter.BevelOuter := bvNone;
  FCenter.Align := alClient;
  FCenter.ParentColor := True;
  FCenter.OnMouseEnter := FOnAppStoreMouseEnter;
  FCenter.OnMouseLeave := FOnAppStoreMouseLeave;
  FCenter.OnClick := FOnAppStoreMouseClick;
  FCenter.OnDblClick := FOnAppStoreMouseDblClick;

  FLblSize:= TLabel.Create(self);
  FLblSize.Parent := FRight;
  FLblSize.AutoSize := False;
  FLblSize.Width := 73;
  FLblSize.Align := alLeft;
  FLblSize.Alignment := taCenter;
  FLblSize.Layout := tlCenter;
  FLblSize.Transparent := True;
  FLblSize.OnMouseEnter := FOnAppStoreMouseEnter;
  FLblSize.OnMouseLeave := FOnAppStoreMouseLeave;
  FLblSize.OnClick := FOnAppStoreMouseClick;
  FLblSize.OnDblClick := FOnAppStoreMouseDblClick;

  FImgInstall:= TImage.Create(self);
  FImgInstall.Parent := FRight;
  FImgInstall.Transparent := True;
  FImgInstall.Center := True;
  FImgInstall.Align := alLeft;
  FImgInstall.Width := 80;
  FImgInstall.OnMouseEnter := FOnAppStoreMouseEnter;
  FImgInstall.OnMouseLeave := FOnAppStoreMouseLeave;
  FImgInstall.OnClick := FOnAppStoreMouseClick;
  FImgInstall.OnDblClick := FOnAppStoreMouseDblClick;
  
  FBtnDown:= TCnFMButton.Create(self);
  FBtnDown.Parent := FRight;
  FBtnDown.Left := 163;
  FBtnDown.Top := 11;
  FBtnDown.Height := 22;
  FBtnDown.Width := 67;
  FBtnDown.Font.Color := clWhite;
  FBtnDown.Font.Style := [fsBold];
  FBtnDown.Bitmaps.Up.LoadFromResourceName(HInstance, '_BTN');
  FBtnDown.Bitmaps.Hot.LoadFromResourceName(HInstance, '_BTN_HIGH');
  FBtnDown.Bitmaps.Disabled.LoadFromResourceName(HInstance, '_BTN_DISABLE');
  FBtnDown.OnMouseEnter := FOnAppStoreMouseEnter;
  FBtnDown.OnMouseLeave := FOnAppStoreMouseLeave;
  FBtnDown.OnClick := FOnAppStoreButtonClick;

  FlblName:= TLabel.Create(self);
  FlblName.Parent := FCenter;
  FlblName.AutoSize := False;
  FlblName.ParentColor := True;
  FlblName.Transparent := True;
  FlblName.Width := 280;
  FlblName.Font.Color := clBlack;
  FlblName.Font.Style := [fsBold];
  FlblName.Top := 7;
  FlblName.Left := 3;
  FlblName.Height := 13;
  FlblName.OnMouseEnter := FOnAppStoreMouseEnter;
  FlblName.OnMouseLeave := FOnAppStoreMouseLeave;
  FlblName.OnClick := FOnAppStoreMouseClick;
  FlblName.OnDblClick := FOnAppStoreMouseDblClick;

  FlblDesc:= TLabel.Create(self);
  FlblDesc.Parent := FCenter;
  FlblDesc.AutoSize := False;
  FlblDesc.ParentColor := True;
  FlblDesc.Transparent := True;
  FlblDesc.Width := 280;
  FlblDesc.Font.Color := clGray;
  FlblDesc.Top := 26;
  FlblDesc.Left := 3;
  FlblDesc.Height := 13;
  FlblDesc.OnMouseEnter := FOnAppStoreMouseEnter;
  FlblDesc.OnMouseLeave := FOnAppStoreMouseLeave;
  FlblDesc.OnClick := FOnAppStoreMouseClick;
  FlblDesc.OnDblClick := FOnAppStoreMouseDblClick;
end;

procedure TCnFMAppStore.CreateWnd;
begin
  inherited CreateWnd;
  Caption := EmptyStr;
end;

destructor TCnFMAppStore.Destroy;
begin

  inherited;
end;

procedure TCnFMAppStore.FOnAppStoreButtonClick(Sender: TObject);
begin
  if Assigned(OnButtonClick) then
    OnButtonClick(self, Data);
end;

procedure TCnFMAppStore.FOnAppStoreMouseClick(Sender: TObject);
begin
  if Assigned(OnItemClick) then
    OnItemClick(self, Data);
end;

procedure TCnFMAppStore.FOnAppStoreMouseDblClick(Sender: TObject);
begin
  if Assigned(OnItemDblClick) then
    OnItemDblClick(self, Data);
end;

procedure TCnFMAppStore.FOnAppStoreMouseEnter(Sender: TObject);
begin
  //
  self.Color := $00F9F0EA;
end;

procedure TCnFMAppStore.FOnAppStoreMouseLeave(Sender: TObject);
begin
  //
  self.Color := clWhite;
end;

function TCnFMAppStore.GetAppDesc: string;
begin
  Result := FlblDesc.Caption;
end;

function TCnFMAppStore.GetAppName: string;
begin
  Result := FlblName.Caption;
end;

function TCnFMAppStore.GetAppSize: string;
begin
  Result := FLblSize.Caption;
end;

procedure TCnFMAppStore.SetAppDesc(const Value: string);
begin
  FlblDesc.Caption := Value;
end;

procedure TCnFMAppStore.SetAppName(const Value: string);
begin
  FlblName.Caption := Value;
end;

procedure TCnFMAppStore.SetAppSize(const Value: string);
begin
  FLblSize.Caption := Value;
end;

procedure TCnFMAppStore.SetStatus(const Value: TCnFMAppStatus);
begin
  FStatus := Value;
  case FStatus of
    asNotInstalled:
      begin
        FBtnDown.Caption := '下载';
        FBtnDown.Enabled := True;
        FImgInstall.Picture.Bitmap.LoadFromResourceName(HInstance, '_NOT_INSTALLED');
      end;
    asInstalled:
      begin
        FBtnDown.Caption := '下载';
        FBtnDown.Enabled := False;
        FImgInstall.Picture.Bitmap.LoadFromResourceName(HInstance, '_INSTALLED');
      end;
    asCanUpdate:
      begin
        FBtnDown.Caption := '更新';
        FBtnDown.Enabled := True;
        FImgInstall.Picture.Bitmap.LoadFromResourceName(HInstance, '_UPDATE');
      end;
  end;
end;

end.
