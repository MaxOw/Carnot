module Engine.FontsManager.FreeTypeError
    ( FreeTypeError (..)
    , freeTypeError_fromErrorCode
    , freeTypeError_toErrorMessage
    ) where

import Delude
import qualified Data.Map as Map
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes

data FreeTypeError
    = CannotOpenResource
    | UnknownFileFormat
    | InvalidFileFormat
    | InvalidVersion
    | LowerModuleVersion
    | InvalidArgument
    | UnimplementedFeature
    | InvalidTable
    | InvalidOffset
    | ArrayTooLarge
    | MissingModule
    | MissingProperty

-- glyph/character errors

    | InvalidGlyphIndex
    | InvalidCharacterCode
    | InvalidGlyphFormat
    | CannotRenderGlyph
    | InvalidOutline
    | InvalidComposite
    | TooManyHints
    | InvalidPixelSize

-- handle errors

    | InvalidHandle
    | InvalidLibraryHandle
    | InvalidDriverHandle
    | InvalidFaceHandle
    | InvalidSizeHandle
    | InvalidSlotHandle
    | InvalidCharMapHandle
    | InvalidCacheHandle
    | InvalidStreamHandle

-- driver errors

    | TooManyDrivers
    | TooManyExtensions

-- memory errors

    | OutOfMemory
    | UnlistedObject

-- stream errors

    | CannotOpenStream
    | InvalidStreamSeek
    | InvalidStreamSkip
    | InvalidStreamRead
    | InvalidStreamOperation
    | InvalidFrameOperation
    | NestedFrameAccess
    | InvalidFrameRead

-- raster errors

    | RasterUninitialized
    | RasterCorrupted
    | RasterOverflow
    | RasterNegativeHeight

-- cache errors

    | TooManyCaches

-- TrueType and SFNT errors

    | InvalidOpcode
    | TooFewArguments
    | StackOverflow
    | CodeOverflow
    | BadArgument
    | DivideByZero
    | InvalidReference
    | DebugOpCode
    | ENDFInExecStream
    | NestedDEFS
    | InvalidCodeRange
    | ExecutionTooLong
    | TooManyFunctionDefs
    | TooManyInstructionDefs
    | TableMissing
    | HorizHeaderMissing
    | LocationsMissing
    | NameTableMissing
    | CMapTableMissing
    | HmtxTableMissing
    | PostTableMissing
    | InvalidHorizMetrics
    | InvalidCharMapFormat
    | InvalidPPem
    | InvalidVertMetrics
    | CouldNotFindContext
    | InvalidPostTableFormat
    | InvalidPostTable
    | DEFInGlyfBytecode
    | MissingBitmap

-- CFF, CID, and Type 1 errors

    | SyntaxError
    | StackUnderflow
    | Ignore
    | NoUnicodeGlyphName
    | GlyphTooBig

-- BDF errors

    | MissingStartfontField
    | MissingFontField
    | MissingSizeField
    | MissingFontboundingboxField
    | MissingCharsField
    | MissingStartcharField
    | MissingEncodingField
    | MissingBbxField
    | BbxTooBig
    | CorruptedFontHeader
    | CorruptedFontGlyphs

    | UnknownFreeTypeError
    deriving (Eq, Ord, Show)

freeTypeError_fromErrorCode :: FT_Error -> FreeTypeError
freeTypeError_fromErrorCode code
    = fromMaybe UnknownFreeTypeError $ Map.lookup code ft_error_code_map

freeTypeError_toErrorMessage :: FreeTypeError -> Text
freeTypeError_toErrorMessage code
    = fromMaybe defMsg $ Map.lookup code ft_error_msg_map
    where defMsg = "unknown free type error"

ft_error_code_map :: Map FT_Error FreeTypeError
ft_error_code_map = Map.fromList $ map (\(e, c, _) -> (c, e)) ft_error_codes

ft_error_msg_map :: Map FreeTypeError Text
ft_error_msg_map = Map.fromList $ map (\(e, _, t) -> (e, t)) ft_error_codes

ft_error_codes :: [(FreeTypeError, FT_Error, Text)]
ft_error_codes =
    [ ( CannotOpenResource,                        0x01,
        "cannot open resource" )
    , ( UnknownFileFormat,                         0x02,
        "unknown file format" )
    , ( InvalidFileFormat,                         0x03,
        "broken file" )
    , ( InvalidVersion,                            0x04,
        "invalid FreeType version" )
    , ( LowerModuleVersion,                        0x05,
        "module version is too low" )
    , ( InvalidArgument,                           0x06,
        "invalid argument" )
    , ( UnimplementedFeature,                      0x07,
        "unimplemented feature" )
    , ( InvalidTable,                              0x08,
        "broken table" )
    , ( InvalidOffset,                             0x09,
        "broken offset within table" )
    , ( ArrayTooLarge,                             0x0A,
        "array allocation size too large" )
    , ( MissingModule,                             0x0B,
        "missing module" )
    , ( MissingProperty,                           0x0C,
        "missing property" )

-- glyph/character errors

    , ( InvalidGlyphIndex,                         0x10,
        "invalid glyph index" )
    , ( InvalidCharacterCode,                      0x11,
        "invalid character code" )
    , ( InvalidGlyphFormat,                        0x12,
        "unsupported glyph image format" )
    , ( CannotRenderGlyph,                         0x13,
        "cannot render this glyph format" )
    , ( InvalidOutline,                            0x14,
        "invalid outline" )
    , ( InvalidComposite,                          0x15,
        "invalid composite glyph" )
    , ( TooManyHints,                              0x16,
        "too many hints" )
    , ( InvalidPixelSize,                          0x17,
        "invalid pixel size" )

-- handle errors

    , ( InvalidHandle,                             0x20,
        "invalid object handle" )
    , ( InvalidLibraryHandle,                      0x21,
        "invalid library handle" )
    , ( InvalidDriverHandle,                       0x22,
        "invalid module handle" )
    , ( InvalidFaceHandle,                         0x23,
        "invalid face handle" )
    , ( InvalidSizeHandle,                         0x24,
        "invalid size handle" )
    , ( InvalidSlotHandle,                         0x25,
        "invalid glyph slot handle" )
    , ( InvalidCharMapHandle,                      0x26,
        "invalid charmap handle" )
    , ( InvalidCacheHandle,                        0x27,
        "invalid cache manager handle" )
    , ( InvalidStreamHandle,                       0x28,
        "invalid stream handle" )

-- driver errors

    , ( TooManyDrivers,                            0x30,
        "too many modules" )
    , ( TooManyExtensions,                         0x31,
        "too many extensions" )

-- memory errors

    , ( OutOfMemory,                               0x40,
        "out of memory" )
    , ( UnlistedObject,                            0x41,
        "unlisted object" )

-- stream errors

    , ( CannotOpenStream,                          0x51,
        "cannot open stream" )
    , ( InvalidStreamSeek,                         0x52,
        "invalid stream seek" )
    , ( InvalidStreamSkip,                         0x53,
        "invalid stream skip" )
    , ( InvalidStreamRead,                         0x54,
        "invalid stream read" )
    , ( InvalidStreamOperation,                    0x55,
        "invalid stream operation" )
    , ( InvalidFrameOperation,                     0x56,
        "invalid frame operation" )
    , ( NestedFrameAccess,                         0x57,
        "nested frame access" )
    , ( InvalidFrameRead,                          0x58,
        "invalid frame read" )

-- raster errors

    , ( RasterUninitialized,                       0x60,
        "raster uninitialized" )
    , ( RasterCorrupted,                           0x61,
        "raster corrupted" )
    , ( RasterOverflow,                            0x62,
        "raster overflow" )
    , ( RasterNegativeHeight,                      0x63,
        "negative height while rastering" )

-- cache errors

    , ( TooManyCaches,                             0x70,
        "too many registered caches" )

-- TrueType and SFNT errors

    , ( InvalidOpcode,                             0x80,
        "invalid opcode" )
    , ( TooFewArguments,                           0x81,
        "too few arguments" )
    , ( StackOverflow,                             0x82,
        "stack overflow" )
    , ( CodeOverflow,                              0x83,
        "code overflow" )
    , ( BadArgument,                               0x84,
        "bad argument" )
    , ( DivideByZero,                              0x85,
        "division by zero" )
    , ( InvalidReference,                          0x86,
        "invalid reference" )
    , ( DebugOpCode,                               0x87,
        "found debug opcode" )
    , ( ENDFInExecStream,                          0x88,
        "found ENDF opcode in execution stream" )
    , ( NestedDEFS,                                0x89,
        "nested DEFS" )
    , ( InvalidCodeRange,                          0x8A,
        "invalid code range" )
    , ( ExecutionTooLong,                          0x8B,
        "execution context too long" )
    , ( TooManyFunctionDefs,                       0x8C,
        "too many function definitions" )
    , ( TooManyInstructionDefs,                    0x8D,
        "too many instruction definitions" )
    , ( TableMissing,                              0x8E,
        "SFNT font table missing" )
    , ( HorizHeaderMissing,                        0x8F,
        "horizontal header (hhea) table missing" )
    , ( LocationsMissing,                          0x90,
        "locations (loca) table missing" )
    , ( NameTableMissing,                          0x91,
        "name table missing" )
    , ( CMapTableMissing,                          0x92,
        "character map (cmap) table missing" )
    , ( HmtxTableMissing,                          0x93,
        "horizontal metrics (hmtx) table missing" )
    , ( PostTableMissing,                          0x94,
        "PostScript (post) table missing" )
    , ( InvalidHorizMetrics,                       0x95,
        "invalid horizontal metrics" )
    , ( InvalidCharMapFormat,                      0x96,
        "invalid character map (cmap) format" )
    , ( InvalidPPem,                               0x97,
        "invalid ppem value" )
    , ( InvalidVertMetrics,                        0x98,
        "invalid vertical metrics" )
    , ( CouldNotFindContext,                       0x99,
        "could not find context" )
    , ( InvalidPostTableFormat,                    0x9A,
        "invalid PostScript (post) table format" )
    , ( InvalidPostTable,                          0x9B,
        "invalid PostScript (post) table" )
    , ( DEFInGlyfBytecode,                         0x9C,
        "found FDEF or IDEF opcode in glyf bytecode" )
    , ( MissingBitmap,                             0x9D,
        "missing bitmap in strike" )

-- CFF, CID, and Type 1 errors

    , ( SyntaxError,                               0xA0,
        "opcode syntax error" )
    , ( StackUnderflow,                            0xA1,
        "argument stack underflow" )
    , ( Ignore,                                    0xA2,
        "ignore" )
    , ( NoUnicodeGlyphName,                        0xA3,
        "no Unicode glyph name found" )
    , ( GlyphTooBig,                               0xA4,
        "glyph too big for hinting" )

-- BDF errors

    , ( MissingStartfontField,                     0xB0,
        "`STARTFONT' field missing" )
    , ( MissingFontField,                          0xB1,
        "`FONT' field missing" )
    , ( MissingSizeField,                          0xB2,
        "`SIZE' field missing" )
    , ( MissingFontboundingboxField,               0xB3,
        "`FONTBOUNDINGBOX' field missing" )
    , ( MissingCharsField,                         0xB4,
        "`CHARS' field missing" )
    , ( MissingStartcharField,                     0xB5,
        "`STARTCHAR' field missing" )
    , ( MissingEncodingField,                      0xB6,
        "`ENCODING' field missing" )
    , ( MissingBbxField,                           0xB7,
        "`BBX' field missing" )
    , ( BbxTooBig,                                 0xB8,
        "`BBX' too big" )
    , ( CorruptedFontHeader,                       0xB9,
        "Font header corrupted or missing fields" )
    , ( CorruptedFontGlyphs,                       0xBA,
        "Font glyphs corrupted or missing fields" )
    ]

