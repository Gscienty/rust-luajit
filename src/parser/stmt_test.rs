use crate::code::InterCode;
use crate::state::LuaState;
use crate::utils::Logger;

#[test]
fn test_indexstmt() {
    log::set_logger(&Logger {}).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    let mut state = LuaState::new();
    let ret = state.parse(
        "
            local table = { 1, 2, 3 };
            table[1] = 2;
        ",
    );

    assert!(ret.is_ok());

    let expect = vec![vec![
        // local a = 0;
        InterCode::LOADNIL(0, 0),
        // local b = 1;
        InterCode::LOADNIL(1, 1),
        // local c = 2;
        InterCode::LOADNIL(2, 2),
        // if a + b == 3 then
        InterCode::ADD(3, 0, 1),
    ]];

    let mut off = 0;
    for proto in &state.proto.prop().children_proto {
        println!("#######################");
        let mut ci = 0;
        for c in &proto.prop().codes {
            //assert_eq!(&expect[off][ci], c);
            println!("{}\t{}", ci, c);
            ci += 1;
        }
        off += 1;
    }
}

#[test]
fn test_forlistlocalvarstmt() {
    log::set_logger(&Logger {}).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    let mut state = LuaState::new();
    let ret = state.parse(
        "
            local table = { 1, 2, 3 };
            local a = 0;
            local b = 1;
            local c = 2;

            for key, value in pairs(table) do
                local a = 3;
                local b = a + b + c + key + value;
            end

            local d = 3;
            local e = a + b + c + d;
        ",
    );

    assert!(ret.is_ok());

    let expect = vec![vec![
        // local a = 0;
        InterCode::LOADNIL(0, 0),
        // local b = 1;
        InterCode::LOADNIL(1, 1),
        // local c = 2;
        InterCode::LOADNIL(2, 2),
        // if a + b == 3 then
        InterCode::ADD(3, 0, 1),
    ]];

    let mut off = 0;
    for proto in &state.proto.prop().children_proto {
        println!("#######################");
        let mut ci = 0;
        for c in &proto.prop().codes {
            //assert_eq!(&expect[off][ci], c);
            println!("{}\t{}", ci, c);
            ci += 1;
        }
        off += 1;
    }
}

#[test]
fn test_iflocalvarstmt() {
    log::set_logger(&Logger {}).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    let mut state = LuaState::new();
    let ret = state.parse(
        "
            local a = 0;
            local b = 1;
            local c = 2;

            if a + b == 3 then
                local c = 3;
                local d = a + b + c;
            elseif a + c ~= 3 then
                local b = 3;
                local d = a + b + c;
            else 
                local a = 3;
                local d = a + b + c;
            end

            local d = 3;
            local e = 4;
        ",
    );

    assert!(ret.is_ok());

    let expect = vec![vec![
        // local a = 0;
        InterCode::LOADNIL(0, 0),
        // local b = 1;
        InterCode::LOADNIL(1, 1),
        // local c = 2;
        InterCode::LOADNIL(2, 2),
        // if a + b == 3 then
        InterCode::ADD(3, 0, 1),
    ]];

    let mut off = 0;
    for proto in &state.proto.prop().children_proto {
        println!("#######################");
        let mut ci = 0;
        for c in &proto.prop().codes {
            //assert_eq!(&expect[off][ci], c);
            println!("{}\t{}", ci, c);
            ci += 1;
        }
        off += 1;
    }
}

#[test]
fn test_funclocalvarstmt() {
    log::set_logger(&Logger {}).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    let mut state = LuaState::new();
    let ret = state.parse(
        "
            local a = 0;
            local b = 1;
            local c = 2;
            local d = 3;

            function func(a, c, b)
                local e = 5;

                return a + b + c + d + e;
            end

            local e = 4;
            local f = 5;
        ",
    );

    assert!(ret.is_ok());

    let expect = vec![
        vec![
            // local a = 0
            InterCode::LOADINT(0, 0),
            // local b = 1
            InterCode::LOADINT(1, 1),
            // local c = 2
            InterCode::LOADINT(2, 2),
            // local d = 3
            InterCode::LOADINT(3, 3),
            // function func(a, c, b)
            InterCode::GETUPVAL(4, 0),
            InterCode::CLOSURE(5, 1),
            InterCode::SETFIELD(4, 0, 5, false),
            // local e = 4
            InterCode::LOADINT(4, 4),
            // local e = 5
            InterCode::LOADINT(5, 5),
        ],
        vec![
            // local d = 4
            InterCode::LOADINT(3, 5),
            // return a + b + c + d + e
            InterCode::ADD(4, 0, 2),
            InterCode::ADD(4, 4, 1),
            InterCode::GETUPVAL(5, 1),
            InterCode::ADD(4, 4, 5),
            InterCode::ADD(4, 4, 3),
            InterCode::RETURN1(4),
            InterCode::RETURN0,
        ],
    ];

    let mut off = 0;
    for proto in &state.proto.prop().children_proto {
        println!("#######################");
        let mut ci = 0;
        for c in &proto.prop().codes {
            assert_eq!(&expect[off][ci], c);
            println!("{}\t{}", ci, c);
            ci += 1;
        }
        off += 1;
    }
}

#[test]
fn test_localvarstmt() {
    log::set_logger(&Logger {}).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    let mut state = LuaState::new();
    let ret = state.parse(
        "
            local a = {
                a = 1,
                b = 2,
                c = 3,
                [ 'a' .. 'b' ] = 4,
            };

            local b = {
                4;
                5.0;
                5.1;
                '6' .. 5;
                a;
                7 + 8 * 9;
            };

            local c = 1;
            local d = 'a';
            local e = 1.0;
            local f = c + e;

            local g = a['a'] + b[2];

        ",
    );

    let expect = vec![
        //
        // local a = {
        //  a = 1,
        //  b = 2,
        //  c = 3,
        //  [ 'a' .. 'b' ] = 4,
        // };
        InterCode::NEWTABLE(0, 4, 0, false),
        InterCode::NOP,
        // a = 1
        InterCode::SETFIELD(0, 0, 1, true),
        // b = 2
        InterCode::SETFIELD(0, 2, 3, true),
        // c = 3
        InterCode::SETFIELD(0, 4, 5, true),
        // 'a' .. 'b'
        InterCode::LOADK(1, 6),
        InterCode::LOADK(2, 7),
        InterCode::CONCAT(1, 2),
        // [ 'a' .. 'b' ] = 4
        InterCode::SETTABLE(0, 1, 8, true),
        //
        // local b = {
        //  4;
        //  5.0;
        //  5.1;
        //  '6' .. 5;
        //  a;
        //  7 + 8 * 9
        // };
        InterCode::NEWTABLE(1, 0, 6, false),
        InterCode::NOP,
        // 4
        InterCode::LOADINT(2, 4),
        // 5.0
        InterCode::LOADFLOAT(3, 5),
        // 5.1
        InterCode::LOADK(4, 9),
        // '6' .. 5
        InterCode::LOADK(5, 10),
        InterCode::LOADINT(6, 5),
        InterCode::CONCAT(5, 2),
        // a;
        InterCode::MOVE(6, 0),
        // 7 + 8 * 9
        InterCode::LOADINT(7, 79),
        InterCode::SETLIST(1, 6, 0, false),
        //
        // local c = 1;
        InterCode::LOADINT(2, 1),
        // local d = 'a';
        InterCode::LOADK(3, 11),
        // local e = 1.0
        InterCode::LOADFLOAT(4, 1),
        // local f = c + e;
        InterCode::ADD(5, 2, 4),
        // a['a']
        InterCode::GETFIELD(6, 0, 12),
        // b[2]
        InterCode::LOADK(7, 13),
        InterCode::GETTABLE(7, 1, 7),
        // local g = a['a'] + b[2]
        InterCode::ADD(6, 6, 7),
    ];

    assert!(ret.is_ok());

    for proto in &state.proto.prop().children_proto {
        println!("#######################");
        let mut ci = 0;
        for c in &proto.prop().codes {
            assert_eq!(&expect[ci], c);
            println!("{}\t{}", ci, c);
            ci += 1;
        }
    }
}

#[test]
fn test_dolocalvarstmt() {
    log::set_logger(&Logger {}).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    let mut state = LuaState::new();
    let ret = state.parse(
        "
            local a = 1;
            local b = 2;
            local c = 3;
            do
                local b = 2;
                do
                    local c = a + b;
                end
                local d = a + b + c;
            end
            local d = 3;
            local e = 4;
        ",
    );

    let expect = vec![
        // local a = 1
        InterCode::LOADINT(0, 1),
        // local b = 2
        InterCode::LOADINT(1, 2),
        // local c = 3
        InterCode::LOADINT(2, 3),
        // local b = 2
        InterCode::LOADINT(3, 2),
        // local c = a + b;
        InterCode::ADD(4, 0, 3),
        // local d = a + b + c;
        InterCode::ADD(4, 0, 3),
        // local d = c + b
        InterCode::ADD(4, 4, 2),
        // local d = 3
        InterCode::LOADINT(3, 3),
        // local e = 4
        InterCode::LOADINT(4, 4),
    ];

    assert!(ret.is_ok());

    for proto in &state.proto.prop().children_proto {
        println!("#######################");
        let mut ci = 0;
        for c in &proto.prop().codes {
            println!("{}\t{}", ci, c);
            assert_eq!(&expect[ci], c);
            ci += 1;
        }
    }
}

#[test]
fn test_whilelocalvarstmt() {
    log::set_logger(&Logger {}).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    let mut state = LuaState::new();
    let ret = state.parse(
        "
            local a = 1;
            local b = 2;
            local c = 3;
            while a + b < 10 do
                local b = 2;
                while a + b < c do
                    local c = a + b;
                end
                local d = a + b + c;
            end
            local d = 3;
            local e = 4;
        ",
    );

    let expect = vec![
        // local a = 1
        InterCode::LOADINT(0, 1),
        // local b = 2
        InterCode::LOADINT(1, 2),
        // local c = 3
        InterCode::LOADINT(2, 3),
        // while a + b < 10 do
        InterCode::ADD(3, 0, 1),
        InterCode::LTI(3, 10, true),
        InterCode::JMP(Some(9)),
        // local b = 2
        InterCode::LOADINT(3, 2),
        // while a + b < c do
        InterCode::ADD(4, 0, 3),
        InterCode::LT(4, 2, true),
        InterCode::JMP(Some(2)),
        // local c = a + b
        InterCode::ADD(4, 0, 3),
        // end
        InterCode::JMP(Some(-5)),
        // local d = a + b + c;
        InterCode::ADD(4, 0, 3),
        InterCode::ADD(4, 4, 2),
        // end
        InterCode::JMP(Some(-12)),
        // local d = 3
        InterCode::LOADINT(3, 3),
        // local e = 4
        InterCode::LOADINT(4, 4),
    ];

    assert!(ret.is_ok());

    for proto in &state.proto.prop().children_proto {
        println!("#######################");
        let mut ci = 0;
        for c in &proto.prop().codes {
            println!("{}\t{}", ci, c);
            assert_eq!(&expect[ci], c);
            ci += 1;
        }
    }
}

#[test]
fn test_repeatlocalvarstmt() {
    log::set_logger(&Logger {}).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    let mut state = LuaState::new();
    let ret = state.parse(
        "
            local a = 1;
            local b = 2;
            local c = 3;
            repeat
                local b = 4;
                local c = 5;
                repeat
                    local c = a + b;
                until a + b > c;
                local d = a + b + c;
            until a + b < c;
            local d = 3;
            local e = a + b + c;
        ",
    );

    let expect = vec![
        // local a = 1
        InterCode::LOADINT(0, 1),
        // local b = 2
        InterCode::LOADINT(1, 2),
        // local c = 3
        InterCode::LOADINT(2, 3),
        // local b = 2;
        InterCode::LOADINT(3, 4),
        // local c = 5;
        InterCode::LOADINT(4, 5),
        // local c = a + b
        InterCode::ADD(5, 0, 3),
        // until a + b > c
        InterCode::ADD(6, 0, 3),
        InterCode::LT(5, 6, true),
        InterCode::JMP(Some(-4)),
        // local d = a + b + c
        InterCode::ADD(5, 0, 3),
        InterCode::ADD(5, 5, 4),
        // until a + b < c
        InterCode::ADD(6, 0, 3),
        InterCode::LT(6, 4, true),
        InterCode::JMP(Some(-11)),
        // local d = 3
        InterCode::LOADINT(3, 3),
        // local e = a + b + c
        InterCode::ADD(4, 0, 1),
        InterCode::ADD(4, 4, 2),
    ];

    assert!(ret.is_ok());

    for proto in &state.proto.prop().children_proto {
        println!("#######################");
        let mut ci = 0;
        for c in &proto.prop().codes {
            println!("{}\t{}", ci, c);
            assert_eq!(&expect[ci], c);
            ci += 1;
        }
    }
}
