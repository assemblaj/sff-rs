// An appropriation of decode/encode functions from
// https://github.com/bmarquismarkail/SFFv2/
pub(crate) fn rle5_decode(src: &[u8], width: usize, height: usize) -> Vec<u8> {
    let mut dst_pos: usize = 0;
    let mut src_pos: usize = 4;
    let mut color;
    let mut bytes_used;

    let mut dst = Vec::new();
    dst.resize(width * height, 0);

    while (src_pos as usize) < src.len() {
        if src[src_pos + 1] & 0x80 > 0 {
            color = src[src_pos + 2];
            bytes_used = 3;
        } else {
            color = 0;
            bytes_used = 2;
        }
        for _ in 0..(src[src_pos] + 1) {
            dst[dst_pos] = color;
            dst_pos += 1;
        }
        src_pos += bytes_used;
        let datalength = (src[src_pos - bytes_used + 1] as usize & 0x7F);
        for _ in 0..datalength {
            for _ in 0..(src[src_pos] as usize >> 5) + 1 {
                dst[dst_pos as usize] = (src[src_pos] & 0x1F);
                dst_pos += 1;
            }
            src_pos += 1;
        }
    }
    dst
}

pub(crate) fn rle8_decode(src: &[u8], width: usize, height: usize) -> Vec<u8> {
    let mut dst_pos: u32 = 0;
    let mut src_pos: u32 = 4;
    let mut dst = Vec::new();
    dst.resize(width * height, 0);

    while (src_pos as usize) < src.len() {
        if ((src[src_pos as usize] & 0xC0) == 0x40) {
            for _ in 0..src[src_pos as usize] & 0x3F {
                dst[dst_pos as usize] = src[src_pos as usize + 1];
                dst_pos += 1;
            }
            src_pos += 2;
        } else {
            dst[dst_pos as usize] = src[src_pos as usize];
            dst_pos += 1;
            src_pos += 1;
        }
    }
    dst
}

pub(crate) fn lz5_decode(mut dst: Vec<u8>, src: &[u8], len: usize) -> Vec<u8> {
    let mut dstpos: usize = 0;
    let mut srcpos: usize = 4;
    let mut recbyt: u8 = 0;
    let mut reccount: u8 = 0;

    while (srcpos < src.len()) {
        //read a control packet and process it bit by bit
        //if the bit is 0, it is an RLE Packet, otherwise it is an LZ packet.
        let ctrlbyt: u8 = src[srcpos];
        //println!("{:#010b}", ctrlbyt);
        srcpos += 1;
        for b in 0..8 {
            if (ctrlbyt & (1 << b) == 0) {
                //println!("Process RLE");
                process_rle(&mut dst, src.to_vec(), &mut dstpos, &mut srcpos);
            } else {
                //println!("Process LZ");
                process_lz(
                    &mut dst,
                    src.to_vec(),
                    &mut dstpos,
                    &mut srcpos,
                    &mut recbyt,
                    &mut reccount,
                );
            }
            if (srcpos >= src.len()) {
                break;
            }
        }
    }
    dst
}

fn process_lz(
    dst: &mut Vec<u8>,
    src: Vec<u8>,
    dstpos: &mut usize,
    srcpos: &mut usize,
    recyclebyte: &mut u8,
    recyclecount: &mut u8,
) {
    //Process an LZ Packet by ANDing the first byte by 0x3F.
    if (src[*srcpos] & 0x3F) > 0 {
        //println!("  Process SLZ");
        //if that equation is nonzero, it is a short LZ packet
        process_slz(dst, src, dstpos, srcpos, recyclebyte, recyclecount);
        //else it is a long LZ packet.
    } else {
        //println!("  Process LLZ");
        process_llz(dst, src, dstpos, srcpos);
    }
}

fn process_slz(
    dst: &mut Vec<u8>,
    src: Vec<u8>,
    dstpos: &mut usize,
    srcpos: &mut usize,
    recyclebyte: &mut u8,
    recyclecount: &mut u8,
) {
    //Process a short LZ packet.
    *recyclebyte |= ((src[*srcpos] & 0xC0) >> (*recyclecount * 2));
    *recyclecount += 1;
    //the answer used to determine the short packet is the copy length.
    //check and see if this is the forth short LZ packet being processed.
    if (*recyclecount == 4) {
        //if so, the recycle buffer has the offset of decompressed data for copying.
        lz_copy(
            dst,
            dstpos,
            (*recyclebyte as usize + 1) as usize,
            (src[*srcpos] as usize & 0x3F) as usize + 1,
        );
        *srcpos += 1;
        *recyclecount = 0;
        *recyclebyte = 0;
    }
    //else read another byte. that is the offset of decompressed data for copying.
    else {
        lz_copy(
            dst,
            dstpos,
            (src[*srcpos as usize + 1] as usize + 1),
            (src[*srcpos] as usize & 0x3F) as usize + 1,
        );
        *srcpos += 2;
    }
}

fn process_llz(dst: &mut Vec<u8>, src: Vec<u8>, dstpos: &mut usize, srcpos: &mut usize) {
    //Process a long LZ packet.
    //the byte read before ANDed by 0xC0 is the top 2 bits of the offset
    //read another byte. That is the bottom 8 bits of the offset.
    let offset = ((src[*srcpos] as usize & 0xC0) << 2) | (src[*srcpos + 1] as usize);
    //read one more byte. That is the copy length.
    //Now copy using the offset - 3
    lz_copy(
        dst,
        dstpos,
        offset as usize + 1,
        (src[*srcpos + 2]) as usize + 3,
    );
    *srcpos += 3;
}

fn pos_compare(lhs: &[u8], rhs: &[u8], count: usize) -> bool {
    for x in 0..count {
        if lhs[x] != rhs[x] {
            return count - x > 0;
        }
    }
    return false;
}

fn naive_memcpy(dst: &mut Vec<u8>, dstpos: &mut usize, offset: usize, len: usize) {
    for i in (0..len).rev() {
        dst[*dstpos] = dst[*dstpos - offset];
        *dstpos += 1;
    }
}

fn lz_copy(dst: &mut Vec<u8>, dstpos: &mut usize, offset: usize, len: usize) {
    naive_memcpy(dst, dstpos, offset, len);
}

fn process_rle(dst: &mut Vec<u8>, src: Vec<u8>, dstpos: &mut usize, srcpos: &mut usize) {
    //Process an RLE Packet by ANDing the first byte by 0xC0 and checking if it's 0
    if (src[*srcpos] & 0xE0) > 0 {
        // println!("  Process SRLE");
        process_srle(dst, src, dstpos, srcpos);
    } else {
        // println!("  Process LRLE");
        process_lrle(dst, src, dstpos, srcpos);
    }
}

fn process_srle(dst: &mut Vec<u8>, src: Vec<u8>, dstpos: &mut usize, srcpos: &mut usize) {
    for run in 0..(src[*srcpos] >> 5) {
        dst[*dstpos] = (src[*srcpos] & 0x1F);
        *dstpos += 1;
    }
    *srcpos += 1;
}

fn process_lrle(dst: &mut Vec<u8>, src: Vec<u8>, dstpos: &mut usize, srcpos: &mut usize) {
    for run in 0..(src[*srcpos + 1] as usize + 8) as usize {
        dst[*dstpos] = (src[*srcpos] & 0x1F);
        *dstpos += 1;
    }
    *srcpos += 2;
}
