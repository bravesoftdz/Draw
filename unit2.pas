{
  Draw - An Application for Drawing and Image Manipulation
  Copyright (C) 2013-2016  René Hickersberger

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  FITNESS FOR A PARTICULAR PURPOSE.
}

unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Windows, DateUtils;

type

  { TFormProperties }

  TFormProperties = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonDefault: TButton;
    ButtonApply: TButton;
    CheckGroup1: TCheckGroup;
    Label1: TLabel;
    LabelDPI: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelFileSize: TLabel;
    LabelFileModified: TLabel;
    LabelFileAccessed: TLabel;
    LabelFileCreated: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpinEditWidth: TSpinEdit;
    SpinEditHeight: TSpinEdit;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonDefaultClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormProperties: TFormProperties;

implementation

{$R *.lfm}

{ TFormProperties }

uses Unit1;

function FileTimeToDateTime(fileTime: TFileTime; var dateTime: TDateTime): boolean;
var
  sysTime: TSystemTime;
begin
  Result := FileTimeToSystemTime(fileTime, sysTime);
  if Result then
    dateTime := SystemTimeToDateTime(sysTime);
end;

function  GetFileTimes(const fileName: string; var creationTime, lastAccessTime,
  lastModificationTime: TDateTime): boolean;
var
  fileHandle            : cardinal;
  fsCreationTime        : TFileTime;
  fsLastAccessTime      : TFileTime;
  fsLastModificationTime: TFileTime;
begin
  try
    Result := false;
    fileHandle := CreateFile(PChar(fileName), GENERIC_READ, FILE_SHARE_READ, nil,
      OPEN_EXISTING, 0, 0);
    if fileHandle <> INVALID_HANDLE_VALUE then
    Result :=
        GetFileTime(fileHandle, @fsCreationTime, @fsLastAccessTime,
           @fsLastModificationTime) and
        FileTimeToDateTime(fsCreationTime, creationTime) and
        FileTimeToDateTime(fsLastAccessTime, lastAccessTime) and
        FileTimeToDateTime(fsLastModificationTime, lastModificationTime);
      CloseHandle(fileHandle);
  except on E:Exception do Result := false;
  end;
end;

function GetSizeOfFile(FileName: string): Int64;
var
  Handle: Integer;
begin
  Handle := FileOpen(FileName, fmOpenRead);

  if Handle = -1 then
  begin
    Result := -1;
    exit;
  end
  else try
    Result := FileSeek(Handle, Int64(0), 2);
  finally
    FileClose(Handle);
  end;
end;

function FormatFileSize(AValue: Int64): string;
const
  K = Int64(1024);
  M = K * K;
  G = K * M;
  T = K * G;
begin
  if AValue = -1 then
  begin
    Result := 'N/A';
    exit;
  end;
  if AValue < K then Result := Format ( '%d bytes', [AValue] )
  else if AValue < M then Result := Format ( '%f KB', [AValue / K] )
  else if AValue < G then Result := Format ( '%f MB', [AValue / M] )
  else if AValue < T then Result := Format ( '%f GB', [AValue / G] )
  else Result := Format ( '%f TB', [AValue / T] );
end;

function GetBMPFileDPI(FileName: String): LongInt;
var
  Stream: TFileStream;
  Data: Word;
  A: Double;
begin
  try
    Result := 0;
    Stream := TFileStream.Create(FileName,
    fmOpenRead or fmShareDenyWrite);
    Stream.Position := 38;
    if Stream.Read(Data,2) = 2 then
    begin
      A := Data;
      Result := Round(A / 39.370079);
    end;
  finally
    Stream.Free;
  end;
end;

function GetFileDPI(filename: String): LongInt;
var Extension: String;
begin
  Extension := ExtractFileExt(filename);

  case UpperCase(Extension) of
    '.BMP': Result := GetBMPFileDPI(filename);
    '.GIF': Result := 96;
    else Result := -1;
  end;
end;

procedure TFormProperties.FormShow(Sender: TObject);
var
  fileName : string;
  attrs    : Integer;
  DPI      : Integer;

  created, lastAccessed,lastModified: TDateTime;
begin
  fileName := FormMain.FileName;

  // Get the file attributes
  attrs := filegetattr(fileName);

  CheckGroup1.Items.Clear;

  if FileExists(FileName) then
  begin

    // Display these attributes
    if attrs and faReadOnly > 0 then
    begin
      CheckGroup1.Checked[CheckGroup1.Items.Add('Read &only')] := True;
    end
    else CheckGroup1.Checked[CheckGroup1.Items.Add('Read &only')] := False;

    if attrs and faHidden > 0 then
    begin
      CheckGroup1.Checked[CheckGroup1.Items.Add('Hi&dden')] := True;
    end
    else CheckGroup1.Checked[CheckGroup1.Items.Add('Hi&dden')] := False;

    if attrs and faArchive > 0 then
    begin
      CheckGroup1.Checked[CheckGroup1.Items.Add('&Archived')] := True;
    end
    else CheckGroup1.Checked[CheckGroup1.Items.Add('&Archived')] := False;
  end else
  begin
    CheckGroup1.Visible := False;
  end;

  if not GetFileTimes(fileName, created, lastAccessed, lastModified) then
  begin
    LabelFileCreated.Caption := 'N/A';
    LabelFileAccessed.Caption := 'N/A';
    LabelFileModified.Caption := 'N/A';
  end else
  begin
    LabelFileCreated.Caption := DateToStr(created);
    LabelFileAccessed.Caption := DateToStr(lastAccessed);
    LabelFileModified.Caption := DateToStr(lastModified);
  end;

  LabelFileSize.Caption := FormatFileSize(GetSizeOfFile(fileName));
  DPI := GetFileDPI(fileName);
  if DPI = -1 then
  begin
    LabelDPI.Caption := 'N/A';
  end else
  begin
    LabelDPI.Caption := IntToStr(DPI) + ' Dots per inch';
  end;

  try
    SpinEditWidth.Value := FormMain.Image1.Picture.Width;
  except on E:Exception do SpinEditWidth.Caption:='N/A';
  end;
  try
    SpinEditHeight.Value := FormMain.Image1.Picture.Height;
  except on E:Exception do SpinEditHeight.Caption:='N/A';
  end;
end;

procedure TFormProperties.ButtonDefaultClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to reset to default? It could destroy your image!', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes then
  begin
    SpinEditWidth.Value := FormMain.Width / 2;
    SpinEditHeight.Value := FormMain.Height / 2;
  end;
end;

procedure TFormProperties.ButtonOkClick(Sender: TObject);
begin
  ButtonApply.Click;
  Hide;
end;

procedure TFormProperties.ButtonCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormProperties.ButtonApplyClick(Sender: TObject);
var Attr: LongInt;
  FileName: String;
  newWidth, newHeight: integer;
begin
  newWidth := SpinEditWidth.Value;
  newHeight := SpinEditHeight.Value;

  with FormMain.Image1 do
  begin
    Width := newWidth;
    Height := newHeight;
    Picture.Bitmap.SetSize(newWidth,newHeight);
  end;

  FileName := FormMain.FileName;

  if FileExists(FileName) then
  begin
    Attr := 0;
    try
      if CheckGroup1.Checked[0] then Attr := Attr or faReadOnly;
      if CheckGroup1.Checked[1] then Attr := Attr or faHidden;
      if CheckGroup1.Checked[2] then Attr := Attr or faArchive;
    except on E:Exception do ;
    end;

    filesetattr(fileName, Attr)
  end;

  FormMain.updateLayout;
  FormMain.updateGraphics;
end;

end.

