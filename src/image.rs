use png::Decoder;

use crate::{SFFSpriteV1, SFFSpriteV2};

pub(crate) fn decode_png(raw_data: &[u8]) -> Vec<u8> {
    let decoder = png::Decoder::new(raw_data);
    let mut reader = decoder.read_info().unwrap();
    let mut buf = Vec::new();
    buf.resize(reader.output_buffer_size(), 0);
    let info = reader.next_frame(&mut buf).unwrap();
    buf[..info.buffer_size()].to_vec()
}

pub(crate) fn apply_palette_sffv2(image_data: Vec<u8>, palette: Vec<[u8; 4]>) -> Vec<u8> {
    image_data
        .into_iter()
        .flat_map(|i| match i as usize {
            0 => [0u8, 0u8, 0u8, 0u8], // some chars need this, some don't.
            i => palette[i],
        })
        .collect::<Vec<_>>()
}

pub(crate) fn decode_pcx(raw_data: &[u8]) -> (Vec<u8>, (u16, u16)) {
    let mut pcx = pcx::Reader::new(raw_data).unwrap();
    let width = pcx.width() as usize;
    let height = pcx.height() as usize;
    let mut data = vec![0; width * height];
    for row in 0..height {
        pcx.next_row_paletted(&mut data[row * width..row * width + width])
            .unwrap();
    }
    (data, (pcx.width(), pcx.height()))
}

pub(crate) fn apply_palette_sffv1(image_data: Vec<u8>, palette: &[[u8; 4]]) -> Vec<u8> {
    image_data
        .into_iter()
        .flat_map(|i| match i as usize {
            0 => [0, 0, 0, 0],
            i => palette[i],
        })
        .collect::<Vec<_>>()
}
