!function(e){var t={};function n(r){if(t[r])return t[r].exports;var o=t[r]={i:r,l:!1,exports:{}};return e[r].call(o.exports,o,o.exports,n),o.l=!0,o.exports}n.m=e,n.c=t,n.d=function(e,t,r){n.o(e,t)||Object.defineProperty(e,t,{enumerable:!0,get:r})},n.r=function(e){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},n.t=function(e,t){if(1&t&&(e=n(e)),8&t)return e;if(4&t&&"object"==typeof e&&e&&e.__esModule)return e;var r=Object.create(null);if(n.r(r),Object.defineProperty(r,"default",{enumerable:!0,value:e}),2&t&&"string"!=typeof e)for(var o in e)n.d(r,o,function(t){return e[t]}.bind(null,o));return r},n.n=function(e){var t=e&&e.__esModule?function(){return e.default}:function(){return e};return n.d(t,"a",t),t},n.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},n.p="",n(n.s=433)}({433:function(e,t,n){"use strict";n.r(t);firebase.initializeApp({apiKey:"AIzaSyAmFMnmxePzfE77N5OhTz_effPaKXud7As",authDomain:"nethop-5cdd6.firebaseapp.com",databaseURL:"https://nethop-5cdd6.firebaseio.com",projectId:"nethop-5cdd6",storageBucket:"nethop-5cdd6.appspot.com",messagingSenderId:"721121633805",appId:"1:721121633805:web:06add6b351d58836c01744",measurementId:"G-KG79CFPFW4"}),firebase.analytics();var r=firebase.database().ref("users").push().key,o=document.querySelector("#btn-intro-next");o&&o.addEventListener("click",(function(){window.location.href="test.html"})),document.location.hash=r,window.sessionStorage.setItem("curUser-unique-firebaseID",r)}});