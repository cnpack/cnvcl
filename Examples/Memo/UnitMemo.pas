unit UnitMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnMemo, CnSpin, ComCtrls, CnTextControl;

type
  TCnMemoForm = class(TForm)
    PageControl1: TPageControl;
    ts1: TTabSheet;
    lblLeftMargin: TLabel;
    lblRightMargin: TLabel;
    chkShowLineNumber: TCheckBox;
    chkHilightLineNumber: TCheckBox;
    btnChangeFont: TButton;
    grpColors: TGroupBox;
    btnLineBkColor: TButton;
    btnLineNumberColor: TButton;
    btnLineNumberHighlight: TButton;
    dlgFontMemo: TFontDialog;
    dlgColor: TColorDialog;
    tsEditorStringList: TTabSheet;
    mmoEditorStringList: TMemo;
    tsTextControl: TTabSheet;
    Label1: TLabel;
    chkTCLine: TCheckBox;
    btnTCFont: TButton;
    StatusBar1: TStatusBar;
    edtMemoLeftMargin: TEdit;
    edtMemoRightMargin: TEdit;
    udMemoLeftMargin: TUpDown;
    udMemoRightMargin: TUpDown;
    chkShowCaret: TCheckBox;
    lblCaretRow: TLabel;
    lblCaretCol: TLabel;
    edtCaretRow: TEdit;
    edtCaretCol: TEdit;
    udCaretRow: TUpDown;
    udCaretCol: TUpDown;
    chkUseSelection: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure chkShowLineNumberClick(Sender: TObject);
    procedure btnChangeFontClick(Sender: TObject);
    procedure chkHilightLineNumberClick(Sender: TObject);
    procedure seLeftMarginChange(Sender: TObject);
    procedure seRightMarginChange(Sender: TObject);
    procedure btnLineBkColorClick(Sender: TObject);
    procedure btnLineNumberColorClick(Sender: TObject);
    procedure btnLineNumberHighlightClick(Sender: TObject);
    procedure chkTCLineClick(Sender: TObject);
    procedure btnTCFontClick(Sender: TObject);
    procedure chkShowCaretClick(Sender: TObject);
    procedure edtCaretRowChange(Sender: TObject);
    procedure edtCaretColChange(Sender: TObject);
    procedure chkUseSelectionClick(Sender: TObject);
  private
    { Private declarations }
    FMemo: TCnMemo;
    FTextControl: TCnVirtualTextControl;
    procedure TestVirtualClick(Sender: TObject);
    procedure TestVirtualCaretChange(Sender: TObject);
    procedure TestVirtualSelectChange(Sender: TObject);
    procedure UpdateStatusBar;
  public
    { Public declarations }
  end;

var
  CnMemoForm: TCnMemoForm;

implementation

{$R *.DFM}

type
  TCnTestVirtualText = class(TCnVirtualTextControl)
  private

  protected
    procedure DoPaintLine(ScreenLineNumber, LineNumber, HoriCharOffset: Integer;
      LineRect: TRect); override;
    procedure Paint; override;
  public
    procedure PaintCursorFrame;
  end;

procedure TCnMemoForm.FormCreate(Sender: TObject);
begin
  FMemo := TCnMemo.Create(Self);
  FMemo.Left := 16;
  FMemo.Top := 48;
  FMemo.Width := 300;
  FMemo.Height := 200;
  FMemo.Anchors := [akLeft, akRight, akTop, akBottom];

  FMemo.Parent := ts1;
  udMemoLeftMargin.Position := FMemo.LineNumberLeftMargin;
  udMemoRightMargin.Position := FMemo.LineNumberRightMargin;

  FTextControl := TCnTestVirtualText.Create(Self);
  FTextControl.MaxLineCount := 1000;
  FTextControl.Anchors := [akLeft, akTop, akRight, akBottom];
  FTextControl.Height := 400;
  FTextControl.ShowLineNumber := True;
  FTextControl.Font.Name := 'Courier New';
  FTextControl.Parent := tsTextControl;
  FTextControl.OnCaretChange := TestVirtualCaretChange;
  FTextControl.OnSelectChange := TestVirtualSelectChange;
  (FTextControl as TCnTestVirtualText).OnClick := TestVirtualClick;
end;

procedure TCnMemoForm.chkShowLineNumberClick(Sender: TObject);
begin
  FMemo.ShowLineNumber := chkShowLineNumber.Checked;
end;

procedure TCnMemoForm.btnChangeFontClick(Sender: TObject);
begin
  if dlgFontMemo.Execute then
    FMemo.Font := dlgFontMemo.Font;
end;

procedure TCnMemoForm.chkHilightLineNumberClick(Sender: TObject);
begin
  FMemo.HighlightNumber := chkHilightLineNumber.Checked;
end;

procedure TCnMemoForm.seLeftMarginChange(Sender: TObject);
begin
  FMemo.LineNumberLeftMargin := udMemoLeftMargin.Position;
end;

procedure TCnMemoForm.seRightMarginChange(Sender: TObject);
begin
  FMemo.LineNumberRightMargin := udMemoRightMargin.Position;
end;

procedure TCnMemoForm.btnLineBkColorClick(Sender: TObject);
begin
  if dlgColor.Execute then
    FMemo.LineNumberBkColor := dlgColor.Color;
end;

procedure TCnMemoForm.btnLineNumberColorClick(Sender: TObject);
begin
  if dlgColor.Execute then
    FMemo.LineNumberColor := dlgColor.Color;
end;

procedure TCnMemoForm.btnLineNumberHighlightClick(Sender: TObject);
begin
  if dlgColor.Execute then
    FMemo.HighlightNumberColor := dlgColor.Color;
end;

procedure TCnMemoForm.chkTCLineClick(Sender: TObject);
begin
  FTextControl.ShowLineNumber := chkTCLine.Checked;
end;

procedure TCnMemoForm.btnTCFontClick(Sender: TObject);
begin
  if dlgFontMemo.Execute then
    FTextControl.Font := dlgFontMemo.Font;
end;

{ TCnTestVirtualText }

procedure TCnTestVirtualText.DoPaintLine(ScreenLineNumber, LineNumber,
  HoriCharOffset: Integer; LineRect: TRect);
var
  S: string;
begin
  S := '=== *** ' + IntToStr(ScreenLineNumber) + ' - ' + IntToStr(LineNumber);
  if HoriCharOffset > 0 then
    Delete(S, 1, HoriCharOffset);
  Canvas.TextOut(LineRect.Left, LineRect.Top, S);
end;

procedure TCnMemoForm.TestVirtualClick(Sender: TObject);
begin
  FTextControl.Invalidate;
end;

procedure TCnTestVirtualText.Paint;
begin
  inherited;
  PaintCursorFrame;
end;

procedure TCnTestVirtualText.PaintCursorFrame;
var
  R: TRect;
begin
  GetScreenCharPosRect(CharFrameRow, CharFrameCol, R);
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clRed;
  Canvas.FillRect(R);
end;

procedure TCnMemoForm.chkShowCaretClick(Sender: TObject);
begin
  FTextControl.UseCaret := chkShowCaret.Checked;
end;

procedure TCnMemoForm.edtCaretRowChange(Sender: TObject);
begin
  if FTextControl <> nil then
    FTextControl.CaretRow := StrToInt(edtCaretRow.Text);
end;

procedure TCnMemoForm.edtCaretColChange(Sender: TObject);
begin
  if FTextControl <> nil then
    FTextControl.CaretCol := StrToInt(edtCaretCol.Text);
end;

procedure TCnMemoForm.TestVirtualCaretChange(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TCnMemoForm.chkUseSelectionClick(Sender: TObject);
begin
  FTextControl.UseSelection := chkUseSelection.Checked;
end;

procedure TCnMemoForm.UpdateStatusBar;
begin
  if FTextControl.HasSelection then
    StatusBar1.SimpleText := Format('Line: %d. Col: %d.  ScreenLine %d. Screen Col %d. Selection from %d/%d to %d/%d',
      [FTextControl.CaretRow, FTextControl.CaretCol, FTextControl.ScreenCaretRow,
       FTextControl.ScreenCaretCol, FTextControl.SelectStartRow, FTextControl.SelectStartCol,
       FTextControl.SelectEndRow, FTextControl.SelectEndCol])
  else
    StatusBar1.SimpleText := Format('Line: %d. Col: %d.  ScreenLine %d. Screen Col %d. No Selection %d/%d',
      [FTextControl.CaretRow, FTextControl.CaretCol, FTextControl.ScreenCaretRow,
       FTextControl.ScreenCaretCol, FTextControl.SelectStartRow, FTextControl.SelectStartCol]);
end;

procedure TCnMemoForm.TestVirtualSelectChange(Sender: TObject);
begin
  UpdateStatusBar;
end;

end.
