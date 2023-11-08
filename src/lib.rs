use std::hash::Hash;
use std::string::ParseError;

use winnow::combinator::{fold_repeat, repeat};
use winnow::prelude::*;
use winnow::stream::StreamIsPartial;
use winnow::token::take;
use winnow::{binary::*, Parser};

use std::collections::HashMap;

mod compression;
mod image;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SpriteId {
    pub group: u16,
    pub image: u16,
}

impl From<(u16, u16)> for SpriteId {
    fn from((group, image): (u16, u16)) -> Self {
        SpriteId { group, image }
    }
}

impl From<SpriteId> for (u16, u16) {
    fn from(SpriteId { group, image }: SpriteId) -> Self {
        (group, image)
    }
}

#[derive(Debug, PartialEq)]
pub enum PaletteType {
    Individual = 0,
    Shared = 1,
}

#[derive(Debug)]
pub struct SFFHeaderV1 {
    version: Version,
    group_total: u32,
    image_total: u32,
    next_subfile: u32,
    subfile_header_len: u32,
    palette_type: PaletteType,
}

#[derive(Debug, PartialEq)]
pub enum SFFSpriteFormatV2 {
    Raw,
    Invalid,
    Rle8,
    Rle5,
    Lz5,
    Png8,
    Png24,
    Png32,
}

#[derive(Debug)]
pub struct SFFHeaderV2 {
    version: Version,
    compatver: Version,
    sprite_offset: u32,
    sprite_total: u32,
    palette_offset: u32,
    palette_total: u32,
    literal_data_offset: u32,
    literal_data_length: u32,
    // translate_data i.e tdata = decompress on load
    translated_data_offset: u32,
    translated_data_length: u32,
}

#[derive(Debug)]
struct PalleteeIndex(u64);

#[derive(Debug)]
pub struct SFFSpriteV1 {
    pub len: u32,
    pub axis_x: u16,
    pub axis_y: u16,
    pub group_number: u16,
    pub image_number: u16,
    pub link_index: u16,
    pub image_data: Vec<u8>,
    pub palette_data: Vec<[u8; 4]>,
    pub width: u16,
    pub height: u16,
}

#[derive(Debug)]
pub struct SFFSpriteV2 {
    pub group_number: u16,
    pub item_number: u16,
    pub width: u16,
    pub height: u16,
    pub axis_x: u16,
    pub axis_y: u16,
    pub link_index: u16,
    pub format: SFFSpriteFormatV2,
    pub color_depth: u8,
    pub data_offset: u32,
    pub data_length: u32,
    pub palette_index: u16,
    pub flags: u16,
    pub palette_data: Vec<[u8; 4]>, // so we can convert from sprite to image
    pub image_data: Vec<u8>,
}

#[derive(Debug)]
pub struct SFFPaletteV2 {
    group_number: u16,
    item_number: u16,
    num_cols: u16,
    pub link_index: u16, // if linked
    data_offset: u32,
    data_length: u32, // (0: if linked )
    pub palette_data: Vec<[u8; 4]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Version(u8, u8, u8, u8);

impl From<Version> for (u8, u8, u8, u8) {
    fn from(v: Version) -> Self {
        (v.0, v.1, v.2, v.3)
    }
}

impl From<&[u8]> for Version {
    fn from(buf: &[u8]) -> Version {
        Version(buf[3], buf[2], buf[1], buf[0])
    }
}
#[derive(Debug)]
pub enum Header {
    Version1 { header: SFFHeaderV1 },
    Version2 { header: SFFHeaderV2 },
}

#[derive(Debug)]
pub enum SFF {
    Version1 {
        header: SFFHeaderV1,
        sprite_list: Vec<SFFSpriteV1>,
    },
    Version2 {
        header: SFFHeaderV2,
        palette_list: Vec<SFFPaletteV2>,
        sprite_list: Vec<SFFSpriteV2>,
    },
}

pub struct SFFImage {
    pub id: SpriteId,
    pub axis_x: u16,
    pub axis_y: u16,
    pub width: u32,
    pub height: u32,
    pub image_data: Vec<u8>,
}

pub type SFFMap = HashMap<(u16, u16), SFFImage>;

impl From<SFF> for SFFMap {
    fn from(sff: SFF) -> SFFMap {
        let mut map: HashMap<(u16, u16), SFFImage> = HashMap::new();
        match sff {
            SFF::Version1 {
                header: _,
                sprite_list,
            } => {
                for sprite in sprite_list {
                    map.insert((sprite.group_number, sprite.image_number), sprite.into());
                }
            }
            SFF::Version2 {
                header: _,
                palette_list: _,
                sprite_list,
            } => {
                for sprite in sprite_list {
                    map.insert((sprite.group_number, sprite.item_number), sprite.into());
                }
            }
        }
        map
    }
}

impl From<SFFSpriteV1> for SFFImage {
    fn from(sprite: SFFSpriteV1) -> SFFImage {
        SFFImage {
            id: SpriteId {
                group: sprite.group_number,
                image: sprite.image_number,
            },
            axis_x: sprite.axis_x,
            axis_y: sprite.axis_y,
            width: sprite.width as u32,
            height: sprite.height as u32,
            image_data: sprite.image_data,
        }
    }
}

impl From<SFFSpriteV2> for SFFImage {
    fn from(sprite: SFFSpriteV2) -> SFFImage {
        SFFImage {
            id: SpriteId {
                group: sprite.group_number,
                image: sprite.item_number,
            },
            axis_x: sprite.axis_x,
            axis_y: sprite.axis_y,
            width: sprite.width as u32,
            height: sprite.height as u32,
            image_data: sprite.image_data,
        }
    }
}

#[derive(Debug)]
pub enum DecodeError {
    InvalidData, //(io::Error),
    InvalidHeader,
    InvalidSignature,
    UnsuporttedVersion(Version),
    InvalidPaletteKind,
    PreviousPaletteNotFound,
    LinkedSpriteNotFound {
        sprite_id: SpriteId,
        linked_index: u16,
    },
    ImageCountMismatch {
        expected_count: u32,
        found_count: u32,
    },
}

impl SFF {
    const ELECBYTE_SIGNATURE: &[u8] = b"ElecbyteSpr\0";

    pub fn decode(file_buf: &[u8]) -> Result<SFF, DecodeError> {
        let (_, header) = SFF::decode_header(file_buf)?;

        match header {
            Header::Version1 { header } => SFF::decode_sff_v1(file_buf, header),
            Header::Version2 { header } => SFF::decode_sff_v2(file_buf, header),
        }
    }

    fn decode_header(s: &[u8]) -> Result<(&[u8], Header), DecodeError> {
        let (next, signature) = signature(s).map_err(|e| DecodeError::InvalidSignature)?;
        if !signature.eq(SFF::ELECBYTE_SIGNATURE) {
            return Err(DecodeError::InvalidSignature);
        } else {
            let (next, version) = version(next).map_err(|e| DecodeError::InvalidData)?;
            let sff_version: Version = Version::from(version.as_ref());
            match sff_version {
                Version(1, _, _, _) => SFF::decode_sff_v1_header(next, sff_version),
                Version(2, _, _, _) => SFF::decode_sff_v2_header(next, sff_version),
                _ => return Err(DecodeError::InvalidData),
            }
        }
    }

    fn decode_sff_v2_header(
        s: &[u8],
        header_version: Version,
    ) -> Result<(&[u8], Header), DecodeError> {
        let (_, next) = seek(s, 8)?;
        let (next, compatver_buf) = version(next).map_err(|e| DecodeError::InvalidData)?;
        let compatver: Version = Version::from(compatver_buf.as_ref());
        let (_, next) = seek(next, 8)?;
        let (next, sprite_offset) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, sprite_total) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, palette_offset) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, palette_total) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, ldata_offset) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, ldata_length) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, tdata_offset) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, tdata_length) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        Ok((
            next,
            Header::Version2 {
                header: SFFHeaderV2 {
                    version: header_version,
                    compatver,
                    sprite_offset,
                    sprite_total,
                    palette_offset,
                    palette_total,
                    literal_data_offset: ldata_offset,
                    literal_data_length: ldata_length,
                    translated_data_offset: tdata_offset,
                    translated_data_length: tdata_length,
                },
            },
        ))
    }

    fn decode_sff_v2(file_buf: &[u8], header: SFFHeaderV2) -> Result<SFF, DecodeError> {
        const SPRITE_SIZE_IN_BYTES: u32 = 28;
        const PALETTE_SIZE_IN_BYTES: u32 = 16;

        let mut palette_offset = header.palette_offset as usize;
        let mut palette_list: Vec<SFFPaletteV2> = Vec::new();
        for i in 0..header.palette_total {
            let (_, data_buf) = seek(file_buf, palette_offset as usize)?;
            let (next, group_number) = le_u16.parse_peek(data_buf).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, item_number) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, num_cols) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, link_index) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, data_offset) = le_u32.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, data_length) = le_u32.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;

            let (_, palette_buf) = seek(
                file_buf,
                (data_offset + header.literal_data_offset) as usize,
            )?;

            let palette_data = if data_length == 0 {
                let linked_palette = palette_list
                    .get(link_index as usize)
                    .ok_or(DecodeError::InvalidData)?;
                linked_palette.palette_data.clone()
            } else {
                let mut res = palette_buf[0..data_length as usize]
                    .chunks_exact(4)
                    .enumerate()
                    .map(|(i, b)| {
                        if header.version.2 == 0 {
                            [b[0], b[1], b[2], 255]
                        } else {
                            [b[0], b[1], b[2], b[3]]
                        }
                    })
                    .collect::<Vec<[u8; 4]>>();
                if res.len() < 256 {
                    res.extend(std::iter::repeat([0, 0, 0, 0]).take(256 - res.len()));
                }
                res
            };

            palette_list.push(SFFPaletteV2 {
                group_number,
                item_number,
                num_cols,
                link_index,
                data_offset,
                data_length,
                palette_data,
            });

            palette_offset = (((i + 1) * PALETTE_SIZE_IN_BYTES) + header.palette_offset) as usize;
        }

        let mut sprite_offset: usize = header.sprite_offset as usize;
        let mut sprite_list: Vec<SFFSpriteV2> = Vec::new();
        for i in 0..header.sprite_total {
            let (_, data_buf) = seek(file_buf, sprite_offset)?;
            let (next, group_number) = le_u16.parse_peek(data_buf).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, item_number) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, width) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, height) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, axis_x) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, axis_y) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, link_index) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, image_format) = le_u8.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, color_depth) = le_u8.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            // Into ldata or tdata based on flags
            let (next, data_offset) = le_u32.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            // 0 if linked
            let (next, data_length) = le_u32.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, palette_index) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, flags) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;

            let format = match image_format {
                0 => SFFSpriteFormatV2::Raw,
                1 => SFFSpriteFormatV2::Invalid,
                2 => SFFSpriteFormatV2::Rle8,
                3 => SFFSpriteFormatV2::Rle5,
                4 => SFFSpriteFormatV2::Lz5,
                10 => SFFSpriteFormatV2::Png8,
                11 => SFFSpriteFormatV2::Png24,
                12 => SFFSpriteFormatV2::Png32,
                _ => return Err(DecodeError::InvalidData),
            };

            let image_offset = if flags & 1 == 0 {
                data_offset + header.literal_data_offset
            } else {
                data_offset + header.translated_data_offset
            };

            // skip invalid sprites
            if format == SFFSpriteFormatV2::Invalid {
                sprite_offset = ((i + 1) * SPRITE_SIZE_IN_BYTES + header.sprite_offset) as usize;
                continue;
            }

            let sprite_data = if data_length == 0 {
                let linked_sprite = sprite_list
                    .get(link_index as usize)
                    .ok_or(DecodeError::InvalidData)?;
                linked_sprite.image_data.clone()
            } else {
                let bytes_per_image_size = 0;

                let raw_data = if format == SFFSpriteFormatV2::Raw {
                    file_buf[image_offset as usize..(image_offset + data_length) as usize].to_vec()
                } else {
                    file_buf[(image_offset + bytes_per_image_size) as usize
                        ..(image_offset + data_length - bytes_per_image_size) as usize]
                        .to_vec()
                };

                let unpaletted_image = match format {
                    SFFSpriteFormatV2::Raw => raw_data,
                    SFFSpriteFormatV2::Invalid => {
                        return Err(DecodeError::InvalidData);
                    }
                    SFFSpriteFormatV2::Rle8 => {
                        compression::rle8_decode(raw_data.as_ref(), width as usize, height as usize)
                    }
                    SFFSpriteFormatV2::Rle5 => {
                        compression::rle5_decode(raw_data.as_ref(), width as usize, height as usize)
                    }
                    SFFSpriteFormatV2::Lz5 => {
                        let mut dst = Vec::new();
                        dst.resize((width as usize * height as usize) as usize, 0);
                        compression::lz5_decode(dst, raw_data.as_ref(), raw_data.len())
                        //compression::decode_lz5_image(raw_data)
                    }
                    SFFSpriteFormatV2::Png8
                    | SFFSpriteFormatV2::Png24
                    | SFFSpriteFormatV2::Png32 => {
                        let data: &[u8] = raw_data.as_ref();
                        image::decode_png(&data[4..data.len()])
                    }
                };
                image::apply_palette_sffv2(
                    unpaletted_image,
                    palette_list[palette_index as usize].palette_data.clone(),
                )
            };

            sprite_list.push(SFFSpriteV2 {
                group_number,
                item_number,
                width,
                height,
                axis_x,
                axis_y,
                link_index,
                format,
                color_depth,
                data_offset: image_offset,
                data_length,
                palette_index,
                flags,
                palette_data: palette_list[palette_index as usize].palette_data.clone(),
                image_data: sprite_data,
            });

            sprite_offset = ((i + 1) * SPRITE_SIZE_IN_BYTES + header.sprite_offset) as usize;
        }

        Ok(SFF::Version2 {
            header: header,
            palette_list: palette_list,
            sprite_list: sprite_list,
        })
    }

    fn decode_sff_v1_header(s: &[u8], version: Version) -> Result<(&[u8], Header), DecodeError> {
        let (next, group_total) = le_u32.parse_peek(s).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, image_total) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, next_subfile) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, subfile_header_len) = le_u32.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;
        let (next, pallete_type_raw) = le_u8.parse_peek(next).map_err(
            |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
        )?;

        let palette_type = match pallete_type_raw {
            0 => PaletteType::Individual,
            1 => PaletteType::Shared,
            _ => return Err(DecodeError::InvalidData),
        };

        Ok((
            next,
            Header::Version1 {
                header: SFFHeaderV1 {
                    version,
                    group_total,
                    image_total,
                    next_subfile,
                    subfile_header_len,
                    palette_type,
                },
            },
        ))
    }

    fn decode_sff_v1(file_buf: &[u8], header: SFFHeaderV1) -> Result<SFF, DecodeError> {
        let (_, data_buf) = seek(file_buf, header.next_subfile as usize)?;

        let mut file_offset = header.next_subfile as usize;
        let mut sprite_list: Vec<SFFSpriteV1> = Vec::new();
        let mut previous_palette: Option<Vec<[u8; 4]>> = None;
        let mut shared_palette: Option<Vec<[u8; 4]>> = None;

        for i in 0..header.image_total {
            let (_, data_buf) = seek(file_buf, file_offset)?;

            let (next, next_subfile) = le_u32.parse_peek(data_buf).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, length) = le_u32.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, axis_x) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, axis_y) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, group_number) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, image_number) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, link_index) = le_u16.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;
            let (next, palette) = le_u8.parse_peek(next).map_err(
                |e: winnow::error::ErrMode<winnow::error::ErrorKind>| DecodeError::InvalidData,
            )?;

            let use_last_palette = palette != 0;

            let mut image_data: Vec<u8>;
            let mut unpaletted_image: Option<Vec<u8>> = None;
            let width: u16;
            let height: u16;
            let mut palette_data: Vec<[u8; 4]>;

            if length == 0 {
                let linked_sprite = sprite_list
                    .get(link_index as usize)
                    .ok_or(DecodeError::InvalidData)?;
                image_data = linked_sprite.image_data.clone();
                width = linked_sprite.width;
                height = linked_sprite.height;
            } else {
                let image_offset = file_offset + 0x20;
                if image_offset + length as usize > file_buf.len() {
                    image_data = file_buf[image_offset..].to_vec();
                } else {
                    image_data = file_buf[image_offset..image_offset + length as usize].to_vec();
                }
                let (image, (w, h)) = image::decode_pcx(image_data.as_ref());
                width = w;
                height = h;
                unpaletted_image = Some(image);
            }

            let palette_size = if i == 0 { 0 } else { 768 };

            let palette_offset = image_data.len().checked_sub(768).unwrap_or(0);

            palette_data = if use_last_palette {
                if shared_palette.is_some() && group_number == 0 {
                    shared_palette.clone().ok_or(DecodeError::InvalidData)?
                } else {
                    previous_palette.clone().ok_or(DecodeError::InvalidData)?
                }
            } else {
                image_data[palette_offset..palette_offset + (256 * 3)]
                    .chunks_exact(3)
                    .enumerate()
                    .map(|(i, b)| {
                        if i == 0 {
                            [b[0], b[1], b[2], 0]
                        } else {
                            [b[0], b[1], b[2], 255]
                        }
                    })
                    .collect::<Vec<[u8; 4]>>()
            };

            if unpaletted_image.is_some() {
                image_data =
                    image::apply_palette_sffv1(unpaletted_image.unwrap(), palette_data.as_ref());
            }

            if i == 0 {
                //&& header.palette_type == PaletteType::Shared {
                shared_palette.replace(palette_data.clone());
            }
            previous_palette.replace(palette_data.clone());

            sprite_list.push(SFFSpriteV1 {
                len: length,
                axis_x,
                axis_y,
                group_number,
                image_number,
                link_index,
                image_data,
                palette_data,
                width,
                height,
            });

            file_offset = next_subfile as usize;
        }
        Ok(SFF::Version1 {
            header: header,
            sprite_list: sprite_list,
        })
    }
}

fn seek(file_buf: &[u8], offset: usize) -> Result<(&[u8], &[u8]), DecodeError> {
    if file_buf.len() < offset {
        Err(DecodeError::InvalidData)
    } else {
        Ok(file_buf.split_at(offset))
    }
}

fn signature(s: &[u8]) -> IResult<&[u8], &[u8]> {
    take(12usize).parse_peek(s)
}

fn version(s: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (next, version_buf) = fold_repeat(0..=4, le_u8, Vec::new, |mut acc: Vec<u8>, item| {
        acc.push(item);
        acc
    })
    .parse_peek(s)?;
    Ok((next, version_buf))
}
