package fr.esiee.fpic.labs

    abstract class LazyShape:
        def scale(a: Float): LazyShape

        // Parameter-less methods: Two takes:
        def show(): Unit

        lazy val area: Float
        lazy val perimeter: Float
