;; kindness-protocol v1
;; A protocol for spreading kindness and gratitude.
;; through forming a network of community members 
;; Alice ==> (G1) grateful for ==> Bob
;; Bob ==> (G2) grateful for ==> Bob (advocate for yourself yo!!)
;; and thus forming a network of gratitude
;; three hundred forty undecillion two hundred eighty-two decillion three hundred sixty-six nonillion nine hundred twenty octillion nine hundred thirty-eight septillion four hundred sixty-three sextillion four hundred sixty-three quintillion three hundred seventy-four quadrillion six hundred seven trillion four hundred thirty-one billion seven hundred sixty-eight million two hundred eleven thousand four hundred fifty-five


;; byte structure
;; type (1 byte) | lv1 ancestor or lv1 id (16 bytes) | lv2 ancestor lv2 id (16 bytes) | lv3 ancestor lv1 id + lv2 id + lv3 id (optional (64 bytes))

(define-non-fungible-token gratitude (buff 48))


;; constants
;;
(define-constant BUFF_TO_BYTE (list 
                                0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f
                                0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f
                                0x20 0x21 0x22 0x23 0x24 0x25 0x26 0x27 0x28 0x29 0x2a 0x2b 0x2c 0x2d 0x2e 0x2f
                                0x30 0x31 0x32 0x33 0x34 0x35 0x36 0x37 0x38 0x39 0x3a 0x3b 0x3c 0x3d 0x3e 0x3f
                                0x40 0x41 0x42 0x43 0x44 0x45 0x46 0x47 0x48 0x49 0x4a 0x4b 0x4c 0x4d 0x4e 0x4f
                                0x50 0x51 0x52 0x53 0x54 0x55 0x56 0x57 0x58 0x59 0x5a 0x5b 0x5c 0x5d 0x5e 0x5f
                                0x60 0x61 0x62 0x63 0x64 0x65 0x66 0x67 0x68 0x69 0x6a 0x6b 0x6c 0x6d 0x6e 0x6f
                                0x70 0x71 0x72 0x73 0x74 0x75 0x76 0x77 0x78 0x79 0x7a 0x7b 0x7c 0x7d 0x7e 0x7f
                                0x80 0x81 0x82 0x83 0x84 0x85 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f
                                0x90 0x91 0x92 0x93 0x94 0x95 0x96 0x97 0x98 0x99 0x9a 0x9b 0x9c 0x9d 0x9e 0x9f
                                0xa0 0xa1 0xa2 0xa3 0xa4 0xa5 0xa6 0xa7 0xa8 0xa9 0xaa 0xab 0xac 0xad 0xae 0xaf
                                0xb0 0xb1 0xb2 0xb3 0xb4 0xb5 0xb6 0xb7 0xb8 0xb9 0xba 0xbb 0xbc 0xbd 0xbe 0xbf
                                0xc0 0xc1 0xc2 0xc3 0xc4 0xc5 0xc6 0xc7 0xc8 0xc9 0xca 0xcb 0xcc 0xcd 0xce 0xcf
                                0xd0 0xd1 0xd2 0xd3 0xd4 0xd5 0xd6 0xd7 0xd8 0xd9 0xda 0xdb 0xdc 0xdd 0xde 0xdf
                                0xe0 0xe1 0xe2 0xe3 0xe4 0xe5 0xe6 0xe7 0xe8 0xe9 0xea 0xeb 0xec 0xed 0xee 0xef
                                0xf0 0xf1 0xf2 0xf3 0xf4 0xf5 0xf6 0xf7 0xf8 0xf9 0xfa 0xfb 0xfc 0xfd 0xfe 0xff))
(define-constant ERR-NON-TRANSFERABLE u1001)
(define-constant ERR-RESTRICTED u1002)
(define-constant ERR-NOT-FOUND u404)
(define-constant ERR-NOT-AUTHORIZED u401)
(define-constant ERR-INVALID-MODE u403)
(define-constant ERR-ALREADY-FRIENDS u409)
(define-constant ERR-REQUEST-ALREADY-SENT u410)
(define-constant ERR-OUT-OF-BOUNDS-MF u411)
(define-constant ERR-CONTRACT-MAXED-OUT u412)
(define-constant ERR-LEVEL-OUT-OF-RANGE u413)
(define-constant ERR-TOO-SHORT u414)

(define-constant EMPTY-16-BYTES 0x00000000000000000000000000000000)
(define-constant FULL-16-BYTES 0xffffffffffffffffffffffffffffffff)
(define-constant PRIVACY-MODE-OPEN u0)
(define-constant PRIVACY-MODE-RESTRICTED u1)
;; data maps and vars
;;
(define-map nft-meta (buff 48) (string-ascii 256))
;; key: top level nft, value: |--children-count (16 bytes)--|--total-grandchildren-count (32 bytes)--|
(define-map nft-legacy (buff 16) (buff 48))
(define-map members principal {
    privacy-mode: uint
})
(define-map connection-requests { recipient: principal, index: uint } { sender: principal })
(define-map connection-requests-indexer principal uint)
;; to prevent spam
(define-map connection-requests-logger {sender: principal, recipient: principal} bool)
(define-map connections {sender: principal, recipient: principal} {blocked: bool})
(define-map lv1-last-child-id (buff 16) (buff 16))
(define-map lv2-last-child-id (buff 32) (buff 16))




(define-data-var last-token-id (buff 16) EMPTY-16-BYTES)
;; private functions
;;

(define-private (read-buff64-closure (idx uint) (state { offset: uint, data: (buff 48), acc: (buff 64) }))
    (let (
        (byte-data (unwrap-panic (element-at (get data state) (+ idx (get offset state)))))
    )
        (merge state { acc: (unwrap-panic (as-max-len? (concat (get acc state) byte-data) u64)) })
    )
)

(define-private (read-buff64 (data (buff 48)) (offset uint))
    (get acc
        (fold read-buff64-closure
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59 u60 u61 u62 u63)
            { offset: offset, data: data, acc: 0x }
        )
    )
)

(define-private (read-buff32-closure (idx uint) (state { offset: uint, data: (buff 48), acc: (buff 32) }))
    (let (
        (byte-data (unwrap-panic (element-at (get data state) (+ idx (get offset state)))))
    )
        (merge state { acc: (unwrap-panic (as-max-len? (concat (get acc state) byte-data) u32)) })
    )
)

(define-private (read-buff32 (data (buff 48)) (offset uint))
    (get acc
        (fold read-buff32-closure
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31)
            { offset: offset, data: data, acc: 0x }
        )
    )
)



(define-private (read-buff16-closure (idx uint) (state { offset: uint, data: (buff 48), acc: (buff 16) }))
    (let (
        (byte-data (unwrap-panic (element-at (get data state) (+ idx (get offset state)))))
    )
        (merge state { acc: (unwrap-panic (as-max-len? (concat (get acc state) byte-data) u16)) })
    )
)

(define-private (read-buff16 (data (buff 48)) (offset uint))
    (get acc
        (fold read-buff16-closure
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15)
            { offset: offset, data: data, acc: 0x }
        )
    )
)

(define-private (increment-closure (idx uint) (state {is-done: bool, data: (buff 32), acc: (buff 32)}))
    (let (
            (acc (get acc state))
            (data (get data state))
            (is-done (get is-done state))
            (byte (if (is-eq (len data) (len acc)) 0x (unwrap-panic (element-at data (- (- (len data) u1) idx)))))
            (new-byte (if is-done
                        byte
                        (if (is-eq 0xff byte)
                            0x00
                            (unwrap-panic (element-at BUFF_TO_BYTE (+ u1 (unwrap-panic (index-of BUFF_TO_BYTE byte))))))))
            (done (or (is-eq (len data) (+ u1 idx)) is-done (not (is-eq new-byte 0x00)))))

        (merge state { is-done: done, acc:  (unwrap-panic (as-max-len? (if (is-eq (len acc) (len data)) acc (concat new-byte acc)) u32))})))





(define-private (increment (data (buff 32)))
    (get acc
        (fold increment-closure
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31)
            { is-done: false, data: data, acc: 0x})))


;; public functions
;;

(define-read-only (get-last-token-id)
    (ok (get-last-token-id-raw))
)

(define-read-only (get-last-token-id-raw) 
    (var-get last-token-id))

(define-private (get-last-lv1-child-id (parent-id (buff 16))) 
    (default-to EMPTY-16-BYTES (map-get? lv1-last-child-id parent-id)))

(define-private (get-last-lv2-child-id (parent-id (buff 32))) 
    (default-to EMPTY-16-BYTES (map-get? lv2-last-child-id parent-id)))


(define-read-only (get-token-uri-raw (nft-id (buff 48)))
    (match (map-get? nft-meta nft-id) uri 
        (some uri)
        none)
)

(define-read-only (get-token-uri (nft-id (buff 48)))
    (ok (some (unwrap! (get-token-uri-raw nft-id) (err ERR-NOT-FOUND))))
)

(define-read-only (get-multi-token-uri (nft-ids (list 200 (buff 48)))) 
    (ok (map get-token-uri nft-ids)))

(define-read-only (get-owner (nft-id (buff 48)))
    (ok (nft-get-owner? gratitude nft-id))
)

(define-read-only (is-member (account principal)) 
    (match (map-get? members account) status true false)
)

(define-read-only (is-valid-mode (mode uint)) 
    (or 
        (is-eq mode PRIVACY-MODE-OPEN)
        (is-eq mode PRIVACY-MODE-RESTRICTED)
    ))

(define-read-only (is-users-friends (account1 principal) (account2 principal)) 
    (or
        (not (default-to true (get blocked (map-get? connections {sender: account1, recipient: account2}))))
        (not (default-to true (get blocked (map-get? connections {sender: account2, recipient: account1}))))
    ))

(define-read-only (can-receive-gratitude (account principal)) 
    (match (map-get? members account)
        member-data
        (or
            (is-eq (get privacy-mode member-data) PRIVACY-MODE-OPEN)
            (is-users-friends tx-sender account)
        )
        true
    )
)

(define-public (transfer (nft-id uint) (sender principal) (recipient principal))
    (err ERR-NON-TRANSFERABLE)
)


(define-private (general-checks (lv uint) (nft-id (buff 48)) (recipient principal)) 
    (begin 
            (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
            (asserts! (can-receive-gratitude recipient) (err ERR-RESTRICTED))
            (asserts! (is-member recipient) (err ERR-NOT-AUTHORIZED))
            (asserts! (not (is-eq (read-buff16 nft-id (* u16 lv)) FULL-16-BYTES)) (err ERR-CONTRACT-MAXED-OUT))
            (ok true))
)


(define-private (handle-lv0-mint (nft-uri (string-ascii 256)) (recipient principal))
    (let (
            (raw-token-id (get-last-token-id-raw))
            (err-check (match (general-checks u0 raw-token-id recipient) response (ok response) error (err error)))
        )
        (asserts! (is-ok err-check) err-check)
        (var-set last-token-id (unwrap-panic (as-max-len? (increment raw-token-id) u16)))
        (map-insert nft-meta raw-token-id nft-uri)
        (nft-mint? gratitude raw-token-id recipient)
    )
)

(define-private (handle-lv1-mint (parent-id (buff 16)) (nft-uri (string-ascii 256)) (recipient principal))
    (let (
            (raw-token-id (get-last-lv1-child-id parent-id))
            (nft-id (concat parent-id raw-token-id))
            (err-check (match (general-checks u1 nft-id recipient) response (ok response) error (err error)))
            (nft-legacy-old (default-to (concat EMPTY-16-BYTES (concat EMPTY-16-BYTES EMPTY-16-BYTES)) (map-get? nft-legacy parent-id)))
            (children-count (read-buff16 nft-legacy-old u0))
            (grand-children-count (read-buff32 nft-legacy-old u16))
            (nft-legacy-new (concat (unwrap-panic (as-max-len? (increment children-count) u16)) grand-children-count))
        )
        (asserts! (is-ok err-check) err-check)
        (map-insert nft-meta nft-id nft-uri)
        (map-set nft-legacy parent-id nft-legacy-new)
        (map-set lv1-last-child-id parent-id (unwrap-panic (as-max-len? (increment raw-token-id) u16)))
        (nft-mint? gratitude nft-id recipient)
    )
)

(define-private (handle-lv2-mint (parent-id (buff 32)) (nft-uri (string-ascii 256)) (recipient principal))
    (let (
            (raw-token-id (get-last-lv2-child-id parent-id))
            (nft-id (concat parent-id raw-token-id))
            (err-check (match (general-checks u2 nft-id recipient) response (ok response) error (err error)))
            (ancestor-id (read-buff16 parent-id u0))
            (nft-legacy-old (default-to (concat EMPTY-16-BYTES (concat EMPTY-16-BYTES EMPTY-16-BYTES)) (map-get? nft-legacy ancestor-id)))
            (children-count (read-buff16 nft-legacy-old u0))
            (grand-children-count (read-buff32 nft-legacy-old u16))
            (nft-legacy-new (concat children-count (unwrap-panic (as-max-len? (increment grand-children-count) u32))))
        )
        (asserts! (is-ok err-check) err-check)
        (map-insert nft-meta nft-id nft-uri)
        (map-set nft-legacy ancestor-id nft-legacy-new)
        (map-set lv2-last-child-id parent-id (unwrap-panic (as-max-len? (increment raw-token-id) u16)))
        (nft-mint? gratitude nft-id recipient)
    )
)

(define-public (mint (parent-id (optional (buff 48))) (nft-uri (string-ascii 256)) (recipient principal)) 
    (let 
        (   
            (nft-level (match parent-id id (/ (len id) u16) u0))
        )
        (asserts! (or (is-none parent-id) (is-some (get-token-uri-raw (unwrap-panic parent-id)))) (err ERR-NOT-FOUND))
        (unwrap-panic (if (is-member recipient) (ok true) (invite-member recipient)))
        (asserts! (> (len nft-uri) u0) (err ERR-TOO-SHORT))
        (asserts! (<= nft-level u2) (err ERR-LEVEL-OUT-OF-RANGE))
        (if (is-eq nft-level u0) (handle-lv0-mint nft-uri recipient) 
            (let 
                (
                    (parent (unwrap-panic parent-id))
                )
                (if (is-eq nft-level u1) 
                    (handle-lv1-mint (unwrap-panic (as-max-len? parent u16)) nft-uri recipient)
                    (if (is-eq nft-level u2)
                        (handle-lv2-mint (unwrap-panic (as-max-len? parent u32)) nft-uri recipient) 
                        (err ERR-LEVEL-OUT-OF-RANGE)
                    ))))
    )
)

(define-public (invite-member (account principal)) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-member tx-sender) (err ERR-NOT-AUTHORIZED))
        (asserts! (not (is-member account)) (err ERR-NOT-AUTHORIZED))
        (ok (map-insert members account {privacy-mode: PRIVACY-MODE-OPEN})))
)

(define-public (burn (nft-id (buff 48))) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (match (nft-get-owner? gratitude nft-id) owner 
            (is-eq tx-sender owner) false)
        (err ERR-NOT-AUTHORIZED))
        (nft-burn? gratitude nft-id tx-sender))
)

(define-public (set-privacy-mode (mode uint)) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-member tx-sender) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-valid-mode mode) (err ERR-INVALID-MODE))
        (map-set members tx-sender {privacy-mode: mode})
        (ok true))
)

(define-public (add-friend (account principal)) 
    (let 
        (
            (current-index (default-to u0 (map-get? connection-requests-indexer tx-sender)))
        )
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (and (is-member account) (is-member tx-sender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (not (is-users-friends account tx-sender)) (err ERR-ALREADY-FRIENDS))
        (asserts! (is-none (map-get? connection-requests-logger {sender: tx-sender, recipient: account})) (err ERR-REQUEST-ALREADY-SENT))
        (map-set connection-requests-indexer tx-sender (+ u1 current-index))
        (map-set connection-requests  {recipient: account, index: current-index} {sender: tx-sender})
        (ok true))
)

(define-public (accept-friend (account principal) (index uint)) 
    (let 
        (
            (current-index (default-to u0 (map-get? connection-requests-indexer tx-sender)))
        )
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (and (is-member account) (is-member tx-sender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-some (map-get? connection-requests {recipient: account, index: index})) (err ERR-NOT-FOUND))
        (asserts! (not (is-users-friends account tx-sender)) (err ERR-ALREADY-FRIENDS))
        (asserts! (> current-index index) (err ERR-OUT-OF-BOUNDS-MF))
        (map-set connections {sender: account, recipient: tx-sender} {blocked: false})
        (map-delete connection-requests  {recipient: tx-sender, index: index})
        (map-delete connection-requests-logger {sender: account, recipient: tx-sender})
        (ok true))
)




(map-insert members tx-sender {privacy-mode: PRIVACY-MODE-OPEN})


