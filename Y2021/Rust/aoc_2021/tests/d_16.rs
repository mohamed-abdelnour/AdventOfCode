use aoc_2021::puzzles::d_16::{Packet, D16};

mod util;
use util::PuzzleExt;

fn version_sum(packet: &str) -> u64 {
    let mut packet = packet.parse::<Packet>().unwrap();
    packet.eval();
    packet.sum
}

#[test]
fn part_1() {
    let tests = [
        ("8A004A801A8002F478", 16),
        ("620080001611562C8802118E34", 12),
        ("C0015000016115A2E0802F182340", 23),
        ("A0016C880162017C3686B18A3D4780", 31),
    ];

    for (packet, sum) in tests {
        assert_eq!(version_sum(packet), sum);
    }
}

fn eval(packet: &str) -> u64 {
    packet.parse::<Packet>().unwrap().eval().unwrap()
}

#[test]
fn part_2() {
    let tests = [
        ("C200B40A82", 3),
        ("04005AC33890", 54),
        ("880086C3E88112", 7),
        ("CE00C43D881120", 9),
        ("D8005AC2A8F0", 1),
        ("F600BC2D8F", 0),
        ("9C005AC2F8F0", 0),
        ("9C0141080250320F1802104A08", 1),
    ];

    for (packet, t) in tests {
        assert_eq!(eval(packet), t);
    }
}

#[test]
fn input() {
    D16.check("D16/input.txt", [847, 333_794_664_059]);
}
