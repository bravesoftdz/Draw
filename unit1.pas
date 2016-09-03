{
  Draw - An Application for Drawing and Image Manipulation
  Copyright (C) 2013-2016  René Hickersberger

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  FITNESS FOR A PARTICULAR PURPOSE.
}

unit Unit1;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PReport, cyColorGrid,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, Printers,
  ComCtrls, StdCtrls, Unit2, FPImage, Windows;

type

  TTool = (Pencil, Brush, Eraser);

  { TFormMain }

  TFormMain = class(TForm)
    ColorDialog1: TColorDialog;
    cyColorGrid1: TcyColorGrid;
    Image1: TImage;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemPrint: TMenuItem;
    MenuItemProperties: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemFile: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelTools: TPanel;
    PanelToolOptions: TPanel;
    PanelColor: TPanel;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButtonEraser: TToolButton;
    ToolButtonPencil: TToolButton;
    ToolButtonBrush: TToolButton;
    procedure cyColorGrid1BoxClick(Sender: TObject; aRow: integer; aCol: integer; aColor: TColor);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemPrintClick(Sender: TObject);
    procedure MenuItemPropertiesClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure PanelColorClick(Sender: TObject);
    procedure ToolButtonBrushClick(Sender: TObject);
    procedure ToolButtonEraserClick(Sender: TObject);
    procedure ToolButtonPencilClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    var
      LastX, LastY: Integer;
      drawing:      boolean;
      DefaultColor: TColor;
      ClearColor:   TColor;
      FileName:     string;
      Tool:         TTool;

    procedure SaveImage(fname: String);

    procedure updateLayout;
    procedure updateGraphics;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if drawing then
  begin
    case Tool of
      TTool.Pencil:
      begin
        Image1.Canvas.Pen.Color := DefaultColor;
        Image1.Canvas.Brush.Color := DefaultColor;
        Image1.Canvas.Line(LastX,LastY,X,Y);
      end;
      TTool.Brush:
      begin
        Image1.Canvas.Pen.Color := DefaultColor;
        Image1.Canvas.Brush.Color := DefaultColor;
        Image1.Canvas.Ellipse(X-15, Y-15, X+15, Y+15);
      end;
      TTool.Eraser:
      begin
        Image1.Canvas.Pen.Color:=ClearColor;
        Image1.Canvas.Brush.Color:=ClearColor;
        Image1.Canvas.Ellipse(X-15, Y-15, X+15, Y+15);
      end;
    end;
  end;
  LastX := X;
  LastY := Y;
end;

procedure TFormMain.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  drawing := false;
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  halt;
end;

procedure TFormMain.MenuItemNewClick(Sender: TObject);
begin
  FileName := '';
  drawing := false;
  with Image1 do
  begin
    Width:=Round(FormMain.Width / 1.8);
    Height:=Round(FormMain.Height / 1.8);
    Picture.Bitmap.SetSize(Width, Height);
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0,0,Image1.Width, Image1.Height);
  end;

  updateLayout;
  updateGraphics;

  ClearColor := clWhite;
end;

procedure TFormMain.MenuItemOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenDialog1.FileName);
    FileName := OpenDialog1.FileName;
    Image1.Width:=Image1.Picture.Width;
    Image1.Height:=Image1.Picture.Height;
  end;

  updateLayout;
  updateGraphics;
end;

procedure TFormMain.MenuItemPrintClick(Sender: TObject);
var
  ScaleX, ScaleY:          Integer;
  RR:                      TRect;
begin
  with Printer do
  begin
    BeginDoc;

    try
      ScaleX := GetDeviceCaps(Handle, logPixelsX) div PixelsPerInch;
      ScaleY := GetDeviceCaps(Handle, logPixelsY) div PixelsPerInch;

      RR := Classes.Rect(0, 0, Image1.Picture.Width * scaleX, Image1.Picture.Height * ScaleY);
      Canvas.StretchDraw(RR, Image1.Picture.Graphic);

      if FileName <> '' then
        Title:=FileName
      else
        Title:='Unnamed picture - Draw';
    finally
      EndDoc;
    end;
  end;
end;

procedure TFormMain.MenuItemPropertiesClick(Sender: TObject);
begin
  FormProperties.Show;
end;

procedure TFormMain.SaveImage(fname: String);
begin
  if FileExists(fname) then
    DeleteFile(PChar(fname));

  case UpperCase(ExtractFileExt(FName)) of
    '.BMP': Image1.Picture.Bitmap.SaveToFile(FName);
    '.PNG': Image1.Picture.PNG.SaveToFile(FName);
    else Image1.Picture.SaveToFile(FName);
  end;
  FileName := FName;
end;

procedure TFormMain.MenuItemSaveAsClick(Sender: TObject);
begin
  SaveDialog1.FileName:=filename;
  if SaveDialog1.Execute then
  begin
    SaveImage(SaveDialog1.FileName);
  end;
end;

procedure TFormMain.MenuItemSaveClick(Sender: TObject);
begin
  if Trim(filename) <> '' then
    SaveImage(FileName)
  else
    MenuItemSaveAs.Click;
end;

procedure TFormMain.PanelColorClick(Sender: TObject);
begin
  ColorDialog1.Color:=cyColorGrid1.Color;
  if ColorDialog1.Execute then
  begin
    DefaultColor := ColorDialog1.Color;
  end;
  updateGraphics;
end;

procedure TFormMain.ToolButtonBrushClick(Sender: TObject);
begin
  Tool := TTool.Brush;
end;

procedure TFormMain.ToolButtonEraserClick(Sender: TObject);
begin
  Tool := TTool.Eraser;
end;

procedure TFormMain.ToolButtonPencilClick(Sender: TObject);
begin
  Tool := TTool.Pencil;
end;

procedure TFormMain.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  drawing := true;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  MenuItemNew.Click;
end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
  updateGraphics;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  updateLayout;
  updateGraphics;
end;

procedure TFormMain.cyColorGrid1BoxClick(Sender: TObject; aRow: integer;
  aCol: integer; aColor: TColor);
begin
  DefaultColor := aColor;
  updateGraphics;
end;

function InvertColor(const Color: TColor): TColor;
begin
  if (GetRValue(Color) + GetGValue(Color) + GetBValue(Color)) > 384 then
    result := clBlack
  else
    result := clWhite;
end;

procedure TFormMain.updateLayout;
begin
  Image1.Left:=Round(Panel1.Width/2 - Image1.Width/2 + Toolbar1.Width / 2 - PanelToolOptions.Width / 2);
  Image1.Top:=Round(Panel1.Height/2 - Image1.Height/2 - cyColorGrid1.Height / 2);
end;

procedure TFormMain.updateGraphics;
begin
  PanelColor.Color:=DefaultColor;
  PanelColor.Font.Color:=InvertColor(DefaultColor);
  PanelColor.Repaint;
end;

end.

