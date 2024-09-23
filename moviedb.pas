unit moviedb;

{$mode Delphi}

interface

uses
  SysUtils, SQLite3Conn, SQLDB, DB, Classes, Dialogs, sqldblib;

type
  TMovieInfo = record
    FileTitle: string;
    Title: string;
    Year: integer;
    Genre: string;
    Actors: string;
    Plot: string;
    Ratings: string;
    Poster: string;
  end;

  TMovieDB = class
  private
    FDatabaseFile: string;
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FLibLoader : TSQLDBLibraryLoader;

    procedure InitializeDatabase;

  public
    constructor Create(const DBFile: string);
    destructor Destroy; override;

    procedure SaveMovieInfo(const MovieInfo: TMovieInfo);
    function GetMovieInfo(const FileTitle: string; Year: Integer; out MovieInfo: TMovieInfo): boolean;
  end;

implementation
{ TMovieDB }

constructor TMovieDB.Create(const DBFile: string);
begin
  FDatabaseFile := DBFile;

  // Create and open the SQLite connection
  FConnection := TSQLite3Connection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Options:=[stoUseImplicit];

  FConnection.DatabaseName := FDatabaseFile;
  FConnection.Transaction := FTransaction;

  // Use TSQLDBLibraryLoader to dynamically load SQLite library
  FLibLoader := TSQLDBLibraryLoader.Create(nil);
  try
    {$IFDEF MSWINDOWS}
    Loader.LibraryName := 'sqlite3.dll'; // Path to SQLite DLL on Windows
    {$ENDIF}
    {$IFDEF LINUX}
    FLibLoader.LibraryName := '/usr/lib/x86_64-linux-gnu/libsqlite3.so.0'; // Adjust path for Linux
    {$ENDIF}
    {$IFDEF DARWIN}
    Loader.LibraryName := '/usr/local/lib/libsqlite3.dylib'; // Path for macOS
    {$ENDIF}

    FLibLoader.ConnectionType := 'SQLite3';
    FLibLoader.Enabled := True;  // Enable loader to load the library
  except
    on E: Exception do
      Writeln('An error occurred: ', E.Message);
  end;

  // Create the database file if it doesn't exist
  InitializeDatabase;
end;

destructor TMovieDB.Destroy;
begin
  // Clean up resources
  FTransaction.Free;
  FConnection.Free;
  FLibLoader.Free;
  inherited;
end;

procedure TMovieDB.InitializeDatabase;
begin
  if not FileExists(FDatabaseFile) then
  begin
    FConnection.Open;
    FTransaction.StartTransaction;
    FConnection.ExecuteDirect(
      'CREATE TABLE IF NOT EXISTS Movies (' +
      'FileTitle TEXT NOT NULL, ' +
      'Title TEXT NOT NULL, ' +
      'Year INTEGER NOT NULL, ' +
      'Genre TEXT, ' +
      'Actors TEXT, ' +
      'Plot TEXT, ' +
      'Ratings TEXT, ' +
      'Poster TEXT, ' +
      'PRIMARY KEY (FileTitle, Year));');
    FTransaction.Commit;
  end
  else
  begin
    FConnection.Open;
  end;
end;

procedure TMovieDB.SaveMovieInfo(const MovieInfo: TMovieInfo);
var
  SQLQuery: TSQLQuery;
begin
  SQLQuery := TSQLQuery.Create(nil);
  try
    // Assign the connection and transaction to the SQLQuery
    SQLQuery.Database := FConnection;
    SQLQuery.Transaction := FTransaction;

    // Prepare the insert query
    SQLQuery.SQL.Text := 'INSERT INTO Movies (FileTitle, Title, Year, Genre, Actors, Plot, Ratings, Poster) ' +
                         'VALUES (:FileTitle, :Title, :Year, :Genre, :Actors, :Plot, :Ratings, :Poster)';

    // Bind the parameters
    SQLQuery.Params.ParamByName('FileTitle').AsString := MovieInfo.FileTitle;
    SQLQuery.Params.ParamByName('Title').AsString := MovieInfo.Title;
    SQLQuery.Params.ParamByName('Year').AsInteger := MovieInfo.Year;
    SQLQuery.Params.ParamByName('Genre').AsString := MovieInfo.Genre;
    SQLQuery.Params.ParamByName('Actors').AsString := MovieInfo.Actors;
    SQLQuery.Params.ParamByName('Plot').AsString := MovieInfo.Plot;
    SQLQuery.Params.ParamByName('Ratings').AsString := MovieInfo.Ratings;
    SQLQuery.Params.ParamByName('Poster').AsString := MovieInfo.Poster;

    try
      // Start the transaction
      FTransaction.StartTransaction;

      // Execute the query
      SQLQuery.ExecSQL;

      // Commit the transaction
      FTransaction.Commit;

      WriteLn('Movie information saved successfully.');

    except
      on E: Exception do
      begin
        // Rollback the transaction if something goes wrong
        if FTransaction.Active then
          FTransaction.Rollback;

        // Show an error message
        WriteLn('An error occurred while saving movie information: ', E.Message);
      end;
    end;

  finally
    SQLQuery.Free;
  end;
end;

function TMovieDB.GetMovieInfo(const FileTitle: string; Year: Integer; out MovieInfo: TMovieInfo): boolean;
var
  SQLQuery: TSQLQuery;
begin
  MovieInfo := Default(TMovieInfo);
  MovieInfo.Title := '';
  Result := False;

  // Check if movie exists in the database
  SQLQuery := TSQLQuery.Create(nil);
  try
    SQLQuery.SQL.Text := 'SELECT * FROM Movies WHERE FileTitle = :FileTitle COLLATE NOCASE';
    if Year >= 1900 then
    begin
      SQLQuery.SQL.Add('AND Year = :Year');;
      SQLQuery.Params.ParamByName('Year').AsInteger := Year;
    end;
    SQLQuery.Params.ParamByName('FileTitle').AsString := FileTitle;

    SQLQuery.DataBase := FConnection;
    SQLQuery.Open;

    if not SQLQuery.EOF then
    begin
      MovieInfo.FileTitle := SQLQuery.FieldByName('FileTitle').AsString;
      MovieInfo.Title := SQLQuery.FieldByName('Title').AsString;
      MovieInfo.Year := SQLQuery.FieldByName('Year').AsInteger;
      MovieInfo.Genre := SQLQuery.FieldByName('Genre').AsString;
      MovieInfo.Actors := SQLQuery.FieldByName('Actors').AsString;
      MovieInfo.Plot := SQLQuery.FieldByName('Plot').AsString;
      MovieInfo.Ratings := SQLQuery.FieldByName('Ratings').AsString;
      MovieInfo.Poster := SQLQuery.FieldByName('Poster').AsString;
      Result := True;
    end;
  finally
    SQLQuery.Free;
  end;
end;

end.

