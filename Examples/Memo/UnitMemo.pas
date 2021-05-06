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
    chkMemoShowCaret: TCheckBox;
    chkMemoUseSelection: TCheckBox;
    btnMemoLoad: TButton;
    dlgOpen1: TOpenDialog;
    chkCaretAfterLineEnd: TCheckBox;
    lblString: TLabel;
    edtString: TEdit;
    mmoColumnIndex: TMemo;
    mmoIndexColumn: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure chkShowLineNumberClick(Sender: TObject);
    procedure btnChangeFontClick(Sender: TObject);
    procedure chkTCLineClick(Sender: TObject);
    procedure btnTCFontClick(Sender: TObject);
    procedure chkShowCaretClick(Sender: TObject);
    procedure edtCaretRowChange(Sender: TObject);
    procedure edtCaretColChange(Sender: TObject);
    procedure chkUseSelectionClick(Sender: TObject);
    procedure chkMemoShowCaretClick(Sender: TObject);
    procedure chkMemoUseSelectionClick(Sender: TObject);
    procedure btnMemoLoadClick(Sender: TObject);
    procedure chkCaretAfterLineEndClick(Sender: TObject);
    procedure edtStringChange(Sender: TObject);
  private
    { Private declarations }
    FMemo: TCnMemo;
    FTextControl: TCnVirtualTextControl;
    procedure TestVirtualClick(Sender: TObject);
    procedure TestVirtualCaretChange(Sender: TObject);
    procedure TestVirtualSelectChange(Sender: TObject);
    procedure UpdateStatusBar;
    procedure CalcIndexColumnMaps;
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
    procedure DoPaintLine(LineCanvas:TCanvas; LineNumber, HoriCharOffset: Integer;
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
  FMemo.Lines.Add('');
  FMemo.Lines.Add('123');
  FMemo.Lines.Add('W啊');
  FMemo.Lines.Add('我吃饭');
  FMemo.Lines.Add(' a c .');

  FMemo.Parent := ts1;

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

  CalcIndexColumnMaps;
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

procedure TCnTestVirtualText.DoPaintLine(LineCanvas: TCanvas; LineNumber,
  HoriCharOffset: Integer; LineRect: TRect);
var
  S, S1: string;
  SSR, SSC, SER, SEC, T: Integer;
begin
  S := '=== *** ' + IntToStr(LineNumber - FVertOffset) + ' - ' + IntToStr(LineNumber) + ' qwertyuiop ASDFGHJKL zxcvbnm,. 0987654321';
//  if HoriCharOffset > 0 then
//    Delete(S, 1, HoriCharOffset);

  if UseSelection and HasSelection then
  begin
    // 判断本行是否在选择区，分五种情况
    // 不在、整行全是、整行左半是、整行右半是、整行中间是
    SSR := SelectStartRow;
    SSC := SelectStartCol;
    SER := SelectEndRow;
    SEC := SelectEndCol;

    if (SER < SSR) or ((SER = SSR) and (SEC < SSC)) then
    begin
      T := SER;
      SER := SSR;
      SSR := T;

      T := SEC;
      SEC := SSC;
      SSC := T;
    end;    // 确保 StartRow/Col 在 EndRow/Col 前面

    if ((LineNumber < SSR) and (LineNumber < SER)) or
      ((LineNumber > SSR) and (LineNumber > SER)) then
    begin
      // 在选择区外，正常画
      LineCanvas.Font.Color := Font.Color;
      LineCanvas.Brush.Style := bsClear;
      LineCanvas.TextOut(LineRect.Left, LineRect.Top, S);
    end
    else if (LineNumber = SSR) and (LineNumber <> SER) then
    begin
      // 等于起始行但不等于结尾行，从 1 到 SSC - 1 画正常区，SSC 后画选择区
      LineCanvas.Font.Color := Font.Color;
      LineCanvas.Brush.Style := bsClear;
      S1 := Copy(S, 1, SSC - 1);
      if S1 <> '' then
      begin
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
        T := LineCanvas.TextWidth(S1);
        Inc(LineRect.Left, T);
      end;

      LineCanvas.Brush.Style := bsSolid;
      LineCanvas.Brush.Color := clHighlight;
      LineCanvas.FillRect(LineRect);
      S1 := Copy(S, SSC, MaxInt);
      if S1 <> '' then
      begin
        LineCanvas.Font.Color := clHighlightText;
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
      end;
    end
    else if (LineNumber = SER) and (LineNumber <> SSR) then
    begin
      // 等于结尾行但不等于起始行，从 1 到 SEC - 1 画选择区，SEC 后画正常区
      S1 := Copy(S, 1, SEC - 1);
      if S1 <> '' then
      begin
        T := LineCanvas.TextWidth(S1);

        LineCanvas.Brush.Style := bsSolid;
        LineCanvas.Brush.Color := clHighlight;

        LineRect.Right := T;
        LineCanvas.FillRect(LineRect);

        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
        Inc(LineRect.Left, T);
      end;
      S1 := Copy(S, SEC, MaxInt);
      if S1 <> '' then
      begin
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.Font.Color := Font.Color;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
      end;
    end
    else if (LineNumber > SSR) and (LineNumber < SER) then
    begin
      // 在选择区内，全画选择色
      LineCanvas.Brush.Style := bsSolid;
      LineCanvas.Brush.Color := clHighlight;
      LineCanvas.FillRect(LineRect);

      LineCanvas.Font.Color := clHighlightText;
      LineCanvas.Brush.Style := bsClear;
      LineCanvas.TextOut(LineRect.Left, LineRect.Top, S);
    end
    else
    begin
      // 在选择行内，从 1 到 SSC - 1 画正常，SSC 到 SEC 中间画选择区，SEC + 1 后画正常
      S1 := Copy(S, 1, SSC - 1);
      if S1 <> '' then   // 画正常区
      begin
        T := LineCanvas.TextWidth(S1);
        LineCanvas.Font.Color := Font.Color;
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
        Inc(LineRect.Left, T);
      end;

      S1 := Copy(S, SSC, SEC - SSC);
      if S1 <> '' then   // 画选择区
      begin
        T := LineCanvas.TextWidth(S1);
        LineCanvas.Brush.Style := bsSolid;
        LineCanvas.Brush.Color := clHighlight;
        LineRect.Right := LineRect.Left + T;
        LineCanvas.FillRect(LineRect);

        LineCanvas.Font.Color := clHighlightText;
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);

        Inc(LineRect.Left, T);
      end;

      S1 := Copy(S, SEC, MaxInt);
      if S1 <> '' then   // 画正常区
      begin
        LineCanvas.Font.Color := Font.Color;
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
      end;
    end;
  end
  else
  begin
    LineCanvas.Font.Color := Font.Color;
    LineCanvas.Brush.Style := bsClear;
    LineCanvas.TextOut(LineRect.Left, LineRect.Top, S);
  end;
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
  GetVirtualCharPosPhysicalRect(CaretRow, CaretCol, R);
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
    StatusBar1.SimpleText := Format('Line: %d. Col: %d. Selection from %d/%d to %d/%d',
      [FTextControl.CaretRow, FTextControl.CaretCol, FTextControl.SelectStartRow, FTextControl.SelectStartCol,
       FTextControl.SelectEndRow, FTextControl.SelectEndCol])
  else
    StatusBar1.SimpleText := Format('Line: %d. Col: %d. No Selection %d/%d',
      [FTextControl.CaretRow, FTextControl.CaretCol, FTextControl.SelectStartRow, FTextControl.SelectStartCol]);
end;

procedure TCnMemoForm.TestVirtualSelectChange(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TCnMemoForm.chkMemoShowCaretClick(Sender: TObject);
begin
  FMemo.UseCaret := chkMemoShowCaret.Checked;
end;

procedure TCnMemoForm.chkMemoUseSelectionClick(Sender: TObject);
begin
  FMemo.UseSelection := chkMemoUseSelection.Checked;
end;

procedure TCnMemoForm.btnMemoLoadClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
    FMemo.LoadFromFile(dlgOpen1.FileName);
end;

procedure TCnMemoForm.chkCaretAfterLineEndClick(Sender: TObject);
begin
  FMemo.CaretAfterLineEnd := chkCaretAfterLineEnd.Checked;
end;

procedure TCnMemoForm.CalcIndexColumnMaps;
var
  S: string;
  I, L, R: Integer;
begin
  mmoColumnIndex.Lines.Clear;
  mmoIndexColumn.Lines.Clear;

  S := edtString.Text;
  for I := -1 to 100 do
  begin
    if MapColumnToCharIndexes(S, I, L, R) then
      mmoColumnIndex.Lines.Add(Format('Col %d: Left Idx %d, Right Idx %d.', [I, L, R]));

    if MapCharIndexToColumns(S, I, L, R) then
      mmoIndexColumn.Lines.Add(Format('Idx %d: Left Col %d, Right Col %d.', [I, L, R]));
  end;
end;

procedure TCnMemoForm.edtStringChange(Sender: TObject);
begin
  CalcIndexColumnMaps;
end;

end.
