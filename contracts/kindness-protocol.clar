;; kindness-protocol v1
;; A protocol for spreading kindness and gratitude.
;; through forming a network of community members 
;; Alice ==> (G1) grateful for ==> Bob
;; Alice ==> (G111) grateful for ==> Charlie ==> (G11) grateful for ==> Alice ==> (G1) grateful for ==> Bob
;; Bob ==> (G2) grateful for ==> Bob (advocate for yourself yo!!)
;; and thus forming a network of gratitude
;; three hundred forty undecillion two hundred eighty-two decillion three hundred sixty-six nonillion 
;; nine hundred twenty octillion nine hundred thirty-eight septillion four hundred sixty-three sextillion 
;; four hundred sixty-three quintillion three hundred seventy-four quadrillion six hundred seven trillion 
;; four hundred thirty-one billion seven hundred sixty-eight million two hundred eleven thousand four hundred fifty-five


;; byte structure
;; type (1 byte) |--lv1 id (16 bytes)--|--lv2 id (optional (16 bytes))--|--lv3 id (optional (16 bytes))--|

(define-non-fungible-token gratitude (buff 48))


;; constants
;;
(define-constant ERR-NON-TRANSFERABLE u1001)
(define-constant ERR-RESTRICTED u1002)
(define-constant ERR-NOT-FOUND u404)
(define-constant ERR-NOT-AUTHORIZED u401)
(define-constant ERR-CONTRACT-MAXED-OUT u412)
(define-constant ERR-LEVEL-OUT-OF-RANGE u413)
(define-constant ERR-TOO-SHORT u414)
(define-constant PRIVACY-MODE-OPEN u0)
(define-constant PRIVACY-MODE-RESTRICTED u1)

(define-constant EMPTY-16-BYTES 0x00000000000000000000000000000000)
(define-constant FULL-16-BYTES 0xffffffffffffffffffffffffffffffff)
;; data maps and vars
;;
(define-map nft-meta (buff 48) (string-ascii 256))
;; key: top level nft, value: |--children-count (16 bytes)--|--total-grandchildren-count (32 bytes)--|
(define-map nft-legacy (buff 16) (buff 48))
(define-map lv1-last-child-id (buff 16) (buff 16))
(define-map lv2-last-child-id (buff 32) (buff 16))




(define-data-var last-token-id (buff 16) EMPTY-16-BYTES)
;; private functions
;;


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
    (ok (unwrap! (nft-get-owner? gratitude nft-id) (err ERR-NOT-FOUND)))
)

(define-read-only (can-receive-gratitude (account principal)) 
    (match (contract-call? .connections get-member-data account)
        member-data
        (or
            (is-eq (get privacy-mode member-data) PRIVACY-MODE-OPEN)
            (contract-call? .connections is-users-friends tx-sender account)
        )
        true
    )
)

(define-public (transfer (nft-id uint) (sender principal) (recipient principal))
    (err ERR-NON-TRANSFERABLE)
)


(define-public (burn (nft-id (buff 48))) 
    (begin 
        (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
        (asserts! (match (nft-get-owner? gratitude nft-id) owner 
            (is-eq tx-sender owner) false)
        (err ERR-NOT-AUTHORIZED))
        (nft-burn? gratitude nft-id tx-sender))
)


(define-private (general-checks (lv uint) (nft-id (buff 48)) (recipient principal)) 
    (begin 
            (asserts! (is-eq tx-sender contract-caller) (err ERR-NOT-AUTHORIZED))
            (asserts! (can-receive-gratitude recipient) (err ERR-RESTRICTED))
            (asserts! (contract-call? .connections is-member recipient) (err ERR-NOT-AUTHORIZED))
            (asserts! (not (is-eq (contract-call? .math read-buff16 nft-id (* u16 lv)) FULL-16-BYTES)) (err ERR-CONTRACT-MAXED-OUT))
            (ok true))
)


(define-private (handle-lv0-mint (nft-uri (string-ascii 256)) (recipient principal))
    (let (
            (raw-token-id (get-last-token-id-raw))
            (err-check (match (general-checks u0 raw-token-id recipient) response (ok response) error (err error)))
        )
        (asserts! (is-ok err-check) err-check)
        (var-set last-token-id (unwrap-panic (as-max-len? (contract-call? .math increment raw-token-id) u16)))
        (map-insert nft-meta raw-token-id nft-uri)
        (nft-mint? gratitude raw-token-id recipient)
    )
)

(define-private (handle-lv1-mint (parent-id (buff 16)) (nft-uri (string-ascii 256)))
    (let (
            (raw-token-id (get-last-lv1-child-id parent-id))
            (recipient (unwrap! (get-owner parent-id) (err ERR-NOT-FOUND)))
            (nft-id (concat parent-id raw-token-id))
            (err-check (match (general-checks u1 nft-id recipient) response (ok response) error (err error)))
            (nft-legacy-old (default-to (concat EMPTY-16-BYTES (concat EMPTY-16-BYTES EMPTY-16-BYTES)) (map-get? nft-legacy parent-id)))
            (children-count (contract-call? .math read-buff16 nft-legacy-old u0))
            (grand-children-count (contract-call? .math read-buff32 nft-legacy-old u16))
            (nft-legacy-new (concat (unwrap-panic (as-max-len? (contract-call? .math increment children-count) u16)) grand-children-count))
        )
        (asserts! (is-ok err-check) err-check)
        (map-insert nft-meta nft-id nft-uri)
        (map-set nft-legacy parent-id nft-legacy-new)
        (map-set lv1-last-child-id parent-id (unwrap-panic (as-max-len? (contract-call? .math increment raw-token-id) u16)))
        (nft-mint? gratitude nft-id recipient)
    )
)

(define-private (handle-lv2-mint (parent-id (buff 32)) (nft-uri (string-ascii 256)))
    (let (
            (raw-token-id (get-last-lv2-child-id parent-id))
            (recipient (unwrap! (get-owner parent-id) (err ERR-NOT-FOUND)))
            (nft-id (concat parent-id raw-token-id))
            (err-check (match (general-checks u2 nft-id recipient) response (ok response) error (err error)))
            (ancestor-id (contract-call? .math read-buff16 parent-id u0))
            (nft-legacy-old (default-to (concat EMPTY-16-BYTES (concat EMPTY-16-BYTES EMPTY-16-BYTES)) (map-get? nft-legacy ancestor-id)))
            (children-count (contract-call? .math read-buff16 nft-legacy-old u0))
            (grand-children-count (contract-call? .math read-buff32 nft-legacy-old u16))
            (nft-legacy-new (concat children-count (unwrap-panic (as-max-len? (contract-call? .math increment grand-children-count) u32))))
        )
        (asserts! (is-ok err-check) err-check)
        (map-insert nft-meta nft-id nft-uri)
        (map-set nft-legacy ancestor-id nft-legacy-new)
        (map-set lv2-last-child-id parent-id (unwrap-panic (as-max-len? (contract-call? .math increment raw-token-id) u16)))
        (nft-mint? gratitude nft-id recipient)
    )
)

(define-public (mint (parent-id (optional (buff 48))) (nft-uri (string-ascii 256)) (recipient (optional principal))) 
    (let 
        (   
            (nft-level (match parent-id id (/ (len id) u16) u0))
        )
        (asserts! (or (is-none parent-id) (is-some (get-token-uri-raw (unwrap-panic parent-id)))) (err ERR-NOT-FOUND))
        (try! (match recipient rec (if (contract-call? .connections is-member rec) (ok true) (contract-call? .connections invite-member rec)) (ok true)))
        (asserts! (> (len nft-uri) u0) (err ERR-TOO-SHORT))
        (asserts! (<= nft-level u2) (err ERR-LEVEL-OUT-OF-RANGE))
        (if (and (is-some recipient) (is-eq nft-level u0)) (handle-lv0-mint nft-uri (unwrap-panic recipient)) 
            (let 
                (
                    (parent (unwrap-panic parent-id))
                )
                (if (is-eq nft-level u1) 
                    (handle-lv1-mint (unwrap-panic (as-max-len? parent u16)) nft-uri)
                    (if (is-eq nft-level u2)
                        (handle-lv2-mint (unwrap-panic (as-max-len? parent u32)) nft-uri) 
                        (err ERR-LEVEL-OUT-OF-RANGE)
                    ))))
    )
)


