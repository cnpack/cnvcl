unit HexEditorUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnHexEditor, ComCtrls, StdCtrls;

type
  TFormHexEditor = class(TForm)
    StatusBarOne: TStatusBar;
    FHexEditor: TCnHexEditor;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure HexEditorSelectionChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormHexEditor: TFormHexEditor;

implementation

{$R *.dfm}

procedure TFormHexEditor.FormCreate(Sender: TObject);
begin
//  FHexEditor := TCnHexEditor.Create(Self);
//  FHexEditor.Parent := Self;
//  FHexEditor.Align := alClient;
//  FHexEditor.Font.Name := 'FixedSys';
  Caption := ParamStr(0);
  FHexEditor.LoadFromFile(ParamStr(0));
  FHexEditor.OnSelectionChange := HexEditorSelectionChange;
end;

procedure TFormHexEditor.HexEditorSelectionChange(Sender: TObject);
begin
  StatusBarOne.Panels[0].Text :=
    Format('SelStart: %d', [TCnHexEditor(Sender).SelStart]);
  StatusBarOne.Panels[1].Text :=
    Format('SelLength: %d', [TCnHexEditor(Sender).SelLength]);
end;

end.
