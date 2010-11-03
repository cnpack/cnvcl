unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CnIniCfg,
  StdCtrls;

type
  TCnIniCfgDemo = class(TCnIniCfg)
  protected
    procedure InitDefaultStrings(Strings: TStringList); override;
  published
    property TestBool: Boolean index 0 read GetBoolean write SetBoolean default False;
    property Enable: Boolean index 1 read GetBoolean write SetBoolean default True;
    property Checked: Boolean index 2 read GetBoolean write SetBoolean default True;

    property TestInteger: Integer index 0 read GetInteger write SetInteger default 100;
    property Width: Integer index 1 read GetInteger write SetInteger default 300;
    property Height: Integer index 2 read GetInteger write SetInteger default 400;

    property TestString: string index 0 read GetString write SetString;
    property Caption: string index 1 read GetString write SetString;
    property Text: string index 2 read GetString write SetString;
  end;

  TForm1 = class(TForm)
    edtTest: TEdit;
    chkEnable: TCheckBox;
    chkChecked: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FCfg: TCnIniCfgDemo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ TCnIniCfgDemo }

procedure TCnIniCfgDemo.InitDefaultStrings(Strings: TStringList);
begin
  inherited;
  Strings.Add('TestString=TestString');
  Strings.Add('Caption=Test Caption');
  Strings.Add('Text=Test Text');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  FCfg := TCnIniCfgDemo.Create(HKEY_CURRENT_USER, 'Software\' +
//    ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  FCfg := TCnIniCfgDemo.Create(ChangeFileExt(ParamStr(0), '.ini'));

  Caption := FCfg.Caption;
  Width := FCfg.Width;
  Height := FCfg.Height;
  edtTest.Text := FCfg.Text;
  chkEnable.Checked := FCfg.Enable;
  chkChecked.Checked := FCfg.Checked;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCfg.Caption := Caption;
  FCfg.Width := Width;
  FCfg.Height := Height;
  FCfg.Text := edtTest.Text;
  FCfg.Enable := chkEnable.Checked;
  FCfg.Checked := chkChecked.Checked;
  FCfg.Free;
end;

end.
