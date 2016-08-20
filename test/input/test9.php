<?php

class C {
    static $v1 = 1;
    # old comment
    const v2 = 2;
    // new comment
    function v2 () {
        return "hello from v2! (C)";
    }
    public function __construct() {
        define(self::v2, "to jest self::v2");
    }
}
class D extends C {
    function v2 () {
        return "hello from v2! (D)";
    }
}
