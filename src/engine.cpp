// Compiled search backend.
//
// A line-by-line port of the R search (R/search.R, R/movegen.R,
// R/evaluate.R, R/keys.R). The R engine stays the reference: the tests run
// both backends on the same positions and budgets and require identical
// root scores, bounds, depth, node counts and principal variations. All
// numbering follows R (squares 1..64, docks 1..12, platforms 1..4, pieces
// 1..n, colours 1 white / 2 black, types 1 P .. 6 K) so that every loop can
// be compared with its R original.

#include <Rcpp.h>
#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

using namespace Rcpp;

// ---------------------------------------------------------------- SHA-256

namespace {

const uint32_t K256[64] = {
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2};

inline uint32_t rotr(uint32_t x, int n) { return (x >> n) | (x << (32 - n)); }

std::string sha256_hex(const std::string& msg) {
  uint32_t h[8] = {0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
                   0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19};
  std::vector<unsigned char> data(msg.begin(), msg.end());
  uint64_t bitlen = static_cast<uint64_t>(data.size()) * 8;
  data.push_back(0x80);
  while (data.size() % 64 != 56) data.push_back(0x00);
  for (int i = 7; i >= 0; --i) data.push_back(static_cast<unsigned char>((bitlen >> (i * 8)) & 0xff));
  uint32_t w[64];
  for (size_t off = 0; off < data.size(); off += 64) {
    for (int i = 0; i < 16; ++i) {
      w[i] = (uint32_t(data[off + 4 * i]) << 24) | (uint32_t(data[off + 4 * i + 1]) << 16) |
             (uint32_t(data[off + 4 * i + 2]) << 8) | uint32_t(data[off + 4 * i + 3]);
    }
    for (int i = 16; i < 64; ++i) {
      uint32_t s0 = rotr(w[i - 15], 7) ^ rotr(w[i - 15], 18) ^ (w[i - 15] >> 3);
      uint32_t s1 = rotr(w[i - 2], 17) ^ rotr(w[i - 2], 19) ^ (w[i - 2] >> 10);
      w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }
    uint32_t a = h[0], b = h[1], c = h[2], d = h[3], e = h[4], f = h[5], g = h[6], hh = h[7];
    for (int i = 0; i < 64; ++i) {
      uint32_t S1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25);
      uint32_t ch = (e & f) ^ (~e & g);
      uint32_t t1 = hh + S1 + ch + K256[i] + w[i];
      uint32_t S0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22);
      uint32_t mj = (a & b) ^ (a & c) ^ (b & c);
      uint32_t t2 = S0 + mj;
      hh = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2;
    }
    h[0] += a; h[1] += b; h[2] += c; h[3] += d; h[4] += e; h[5] += f; h[6] += g; h[7] += hh;
  }
  static const char* hex = "0123456789abcdef";
  std::string out(64, '0');
  for (int i = 0; i < 8; ++i) {
    for (int j = 0; j < 8; ++j) out[i * 8 + j] = hex[(h[i] >> (28 - 4 * j)) & 0xf];
  }
  return out;
}

// ---------------------------------------------------------------- tables

const int PIECE_VALUE[7] = {0, 100, 300, 300, 500, 900, 0};
const int PAWN_DIR[3] = {0, 1, -1};
const double MATE = 1000000.0;
const double MATE_BOUND = MATE - 1000.0;
const int QDEPTH = 4;
const char TYPE_CHARS[7] = {' ', 'P', 'N', 'B', 'R', 'Q', 'K'};
const char* PLATFORMS[5] = {"", "WQ", "WK", "BQ", "BK"};
const char* COLORS[3] = {"", "white", "black"};

struct CastleDef {
  std::string king, rook;
  int king_from, rook_from, king_to, rook_to;
  std::vector<int> home_platforms, home_docks, empty;
  std::vector<std::pair<int, int>> clear, probe;
};

struct PromoDef {
  int board, rank, prank;
  std::vector<int> docks;
};

struct Tables {
  std::string rules_hash;
  int sq_board[65];
  std::string sq_id[65];
  std::string dock_id[13];
  int main_file[49], main_rank[49];
  int plat_file[13][3][5], plat_rank[13][3][5];
  std::vector<int> dock_adj[13];
  std::vector<int> main_by_ci[61];
  std::vector<int> knight_ci[61], king_ci[61];
  std::vector<int> ray_ci[61][9];
  PromoDef promo[3];
  CastleDef castle[3][3];
};

std::vector<int> ivec(SEXP x) {
  IntegerVector v(x);
  return std::vector<int>(v.begin(), v.end());
}

Tables* build_tables(List t) {
  Tables* T = new Tables();
  T->rules_hash = as<std::string>(t["rules_hash"]);
  IntegerVector sb = t["sq_board"];
  CharacterVector sid = t["sq_id"];
  for (int s = 1; s <= 64; ++s) {
    T->sq_board[s] = sb[s - 1];
    T->sq_id[s] = as<std::string>(sid[s - 1]);
  }
  CharacterVector did = t["dock_ids"];
  for (int d = 1; d <= 12; ++d) T->dock_id[d] = as<std::string>(did[d - 1]);
  IntegerVector mf = t["main_file"], mr = t["main_rank"];
  for (int s = 1; s <= 48; ++s) {
    T->main_file[s] = mf[s - 1];
    T->main_rank[s] = mr[s - 1];
  }
  IntegerVector pf = t["plat_file"], pr = t["plat_rank"];  // arrays [12, 2, 4], column-major
  for (int d = 1; d <= 12; ++d) {
    for (int o = 1; o <= 2; ++o) {
      for (int c = 1; c <= 4; ++c) {
        int idx = (d - 1) + 12 * (o - 1) + 24 * (c - 1);
        T->plat_file[d][o][c] = pf[idx];
        T->plat_rank[d][o][c] = pr[idx];
      }
    }
  }
  List adj = t["dock_adj"];
  for (int d = 1; d <= 12; ++d) T->dock_adj[d] = ivec(adj[d - 1]);
  List mbc = t["main_by_ci"], kn = t["knight_ci"], kg = t["king_ci"], ray = t["ray_ci"];
  for (int c = 1; c <= 60; ++c) {
    T->main_by_ci[c] = ivec(mbc[c - 1]);
    T->knight_ci[c] = ivec(kn[c - 1]);
    T->king_ci[c] = ivec(kg[c - 1]);
    List rc = ray[c - 1];
    for (int k = 1; k <= 8; ++k) T->ray_ci[c][k] = ivec(rc[k - 1]);
  }
  List promo = t["promo"];
  for (int col = 1; col <= 2; ++col) {
    List p = promo[col - 1];
    T->promo[col].board = as<int>(p["board"]);
    T->promo[col].rank = as<int>(p["rank"]);
    T->promo[col].prank = as<int>(p["prank"]);
    T->promo[col].docks = ivec(p["docks"]);
  }
  List castle = t["castle"];
  for (int col = 1; col <= 2; ++col) {
    List cc = castle[col - 1];
    for (int cs = 1; cs <= 2; ++cs) {
      List d = cc[cs - 1];
      CastleDef& D = T->castle[col][cs];
      D.king = as<std::string>(d["king"]);
      D.rook = as<std::string>(d["rook"]);
      D.king_from = as<int>(d["king_from"]);
      D.rook_from = as<int>(d["rook_from"]);
      D.king_to = as<int>(d["king_to"]);
      D.rook_to = as<int>(d["rook_to"]);
      D.home_platforms = ivec(d["home_platforms"]);
      D.home_docks = ivec(d["home_docks"]);
      D.empty = ivec(d["empty"]);
      IntegerMatrix cl = d["clear"], pb = d["probe"];
      for (int i = 0; i < cl.nrow(); ++i) D.clear.push_back({cl(i, 0), cl(i, 1)});
      for (int i = 0; i < pb.nrow(); ++i) D.probe.push_back({pb(i, 0), pb(i, 1)});
    }
  }
  return T;
}

// ---------------------------------------------------------------- state

struct Core {
  int occ[65];
  std::vector<std::string> pid;  // index 0 unused
  std::vector<int> ptype, porigin, pcolor, psq;
  std::vector<char> pmoved;
  int dock[5], orient[5];
  int side;
  bool castle[5];
  bool first[3];
  bool has_ep;
  int ep[5];
  int halfmove, ply;
  std::vector<std::string> rep;
};

struct Ctx {
  int file[65], rank[65], ci[65], sqcol[65];
  int cnt[61];
  std::vector<int> by_ci[61];
};

struct Act {
  int type, piece, from, to, cap, promo, plat, dock, rot, cside, flag;
};

Core core_from_list(List x) {
  Core c;
  IntegerVector occ = x["occ"];
  for (int s = 1; s <= 64; ++s) c.occ[s] = occ[s - 1];
  CharacterVector pid = x["pid"];
  IntegerVector pt = x["ptype"], po = x["porigin"], pc = x["pcolor"], ps = x["psq"];
  LogicalVector pm = x["pmoved"];
  int n = pid.size();
  c.pid.assign(n + 1, "");
  c.ptype.assign(n + 1, 0);
  c.porigin.assign(n + 1, 0);
  c.pcolor.assign(n + 1, 0);
  c.psq.assign(n + 1, 0);
  c.pmoved.assign(n + 1, 0);
  for (int i = 1; i <= n; ++i) {
    c.pid[i] = as<std::string>(pid[i - 1]);
    c.ptype[i] = pt[i - 1];
    c.porigin[i] = po[i - 1];
    c.pcolor[i] = pc[i - 1];
    c.psq[i] = ps[i - 1];
    c.pmoved[i] = pm[i - 1] ? 1 : 0;
  }
  IntegerVector dk = x["dock"], orr = x["orient"];
  for (int p = 1; p <= 4; ++p) {
    c.dock[p] = dk[p - 1];
    c.orient[p] = orr[p - 1];
  }
  c.side = as<int>(x["side"]);
  LogicalVector cr = x["castle"], fs = x["first"];
  for (int i = 1; i <= 4; ++i) c.castle[i] = cr[i - 1];
  c.first[1] = fs[0];
  c.first[2] = fs[1];
  IntegerVector ep = x["ep"];
  c.has_ep = ep.size() == 4;
  for (int i = 1; i <= 4; ++i) c.ep[i] = c.has_ep ? ep[i - 1] : 0;
  c.halfmove = as<int>(x["halfmove"]);
  c.ply = as<int>(x["ply"]);
  CharacterVector rep = x["rep"];
  for (int i = 0; i < rep.size(); ++i) c.rep.push_back(as<std::string>(rep[i]));
  return c;
}

void square_coords(const Tables& T, const int* dock, const int* orient, int* file, int* rank) {
  for (int s = 1; s <= 48; ++s) {
    file[s] = T.main_file[s];
    rank[s] = T.main_rank[s];
  }
  for (int p = 1; p <= 4; ++p) {
    int o = orient[p] == 180 ? 2 : 1;
    for (int c = 1; c <= 4; ++c) {
      int s = 48 + (p - 1) * 4 + c;
      file[s] = T.plat_file[dock[p]][o][c];
      rank[s] = T.plat_rank[dock[p]][o][c];
    }
  }
}

void make_ctx(const Tables& T, const Core& c, Ctx& x) {
  square_coords(T, c.dock, c.orient, x.file, x.rank);
  for (int i = 1; i <= 60; ++i) x.cnt[i] = 0;
  for (int s = 1; s <= 64; ++s) {
    x.ci[s] = x.file[s] * 10 + x.rank[s] + 1;
    if (c.occ[s] > 0) {
      x.sqcol[s] = c.pcolor[c.occ[s]];
      x.cnt[x.ci[s]]++;
    } else {
      x.sqcol[s] = 0;
    }
  }
  for (int i = 1; i <= 60; ++i) x.by_ci[i] = T.main_by_ci[i];
  for (int k = 1; k <= 16; ++k) x.by_ci[x.ci[48 + k]].push_back(48 + k);
}

int npieces(const Core& c) { return static_cast<int>(c.pid.size()) - 1; }

bool attacked(int f, int r, int by, const int* file, const int* rank, const int* cnt, const Core& c,
              const std::vector<int>& psq) {
  int n = npieces(c);
  for (int i = 1; i <= n; ++i) {
    if (c.pcolor[i] != by || psq[i] <= 0) continue;
    int s = psq[i];
    int t = c.ptype[i];
    int df = f - file[s], dr = r - rank[s];
    int adf = std::abs(df), adr = std::abs(dr);
    if (t == 1) {
      if (dr == PAWN_DIR[by] && adf == 1) return true;
    } else if (t == 2) {
      if ((adf == 1 && adr == 2) || (adf == 2 && adr == 1)) return true;
    } else if (t == 6) {
      if (adf <= 1 && adr <= 1 && adf + adr > 0) return true;
    }
  }
  for (int i = 1; i <= n; ++i) {
    if (c.pcolor[i] != by || psq[i] <= 0) continue;
    int t = c.ptype[i];
    if (t < 3 || t > 5) continue;
    int s = psq[i];
    int df = f - file[s], dr = r - rank[s];
    int adf = std::abs(df), adr = std::abs(dr);
    bool ortho = (t == 4 || t == 5) && ((df == 0) != (dr == 0));
    bool diag = (t == 3 || t == 5) && adf == adr && adf > 0;
    if (!ortho && !diag) continue;
    int nn = std::max(adf, adr);
    if (nn == 1) return true;
    int sf = (df > 0) - (df < 0), sr = (dr > 0) - (dr < 0);
    bool clear = true;
    for (int k = 1; k < nn; ++k) {
      int ci = (file[s] + k * sf) * 10 + rank[s] + k * sr + 1;
      if (cnt[ci] != 0) {
        clear = false;
        break;
      }
    }
    if (clear) return true;
  }
  return false;
}

int king_sq(const Core& c, int color) {
  int found = 0, sq = 0;
  for (int i = 1; i <= npieces(c); ++i) {
    if (c.ptype[i] == 6 && c.pcolor[i] == color && c.psq[i] > 0) {
      ++found;
      sq = c.psq[i];
    }
  }
  return found == 1 ? sq : 0;
}

bool in_check(const Core& c, int color, const Ctx& x) {
  int ks = king_sq(c, color);
  if (ks == 0) return false;
  return attacked(x.file[ks], x.rank[ks], 3 - color, x.file, x.rank, x.cnt, c, c.psq);
}

bool promotes(const Tables& T, int color, int sq, int rank, const int* dock) {
  const PromoDef& pr = T.promo[color];
  int b = T.sq_board[sq];
  if (b <= 3) return b == pr.board && rank == pr.rank;
  int d = dock[b - 3];
  return std::find(pr.docks.begin(), pr.docks.end(), d) != pr.docks.end() && rank == pr.prank;
}

// setdiff(dock_adj[[dock[pl]]], dock[-pl]) in R's order.
std::vector<int> free_docks(const Tables& T, const Core& c, int pl) {
  std::vector<int> out;
  for (int d : T.dock_adj[c.dock[pl]]) {
    bool used = false;
    for (int q = 1; q <= 4; ++q) {
      if (q != pl && c.dock[q] == d) used = true;
    }
    if (!used && std::find(out.begin(), out.end(), d) == out.end()) out.push_back(d);
  }
  return out;
}

void push_moves(std::vector<Act>& out, const Tables& T, const Core& c, const Ctx& x, int p, int s,
                const std::vector<int>& to, const std::vector<int>& flag) {
  if (to.empty()) return;
  int side = c.side;
  std::vector<Act> rows;
  for (size_t i = 0; i < to.size(); ++i) {
    Act a{1, p, s, to[i], c.occ[to[i]], 0, 0, 0, 0, 0, flag[i]};
    if (flag[i] == 2) a.cap = c.ep[1];
    rows.push_back(a);
  }
  if (c.ptype[p] == 1) {
    std::vector<char> pro(rows.size());
    bool any = false;
    for (size_t i = 0; i < rows.size(); ++i) {
      pro[i] = promotes(T, side, rows[i].to, x.rank[rows[i].to], c.dock);
      any = any || pro[i];
    }
    if (any) {
      for (size_t i = 0; i < rows.size(); ++i) if (!pro[i]) out.push_back(rows[i]);
      for (size_t i = 0; i < rows.size(); ++i) {
        if (!pro[i]) continue;
        for (int pt : {5, 4, 3, 2}) {
          Act a = rows[i];
          a.promo = pt;
          out.push_back(a);
        }
      }
      return;
    }
  }
  for (auto& a : rows) out.push_back(a);
}

std::vector<Act> pseudo(const Tables& T, const Core& c, const Ctx& x) {
  std::vector<Act> out;
  int side = c.side, enemy = 3 - side;
  bool has_ep = c.has_ep && c.pcolor[c.ep[1]] != side && c.psq[c.ep[1]] == c.ep[2];
  int ep_ci = has_ep ? c.ep[3] * 10 + c.ep[4] + 1 : -1;
  for (int p = 1; p <= npieces(c); ++p) {
    if (c.pcolor[p] != side || c.psq[p] <= 0) continue;
    int s = c.psq[p], cs = x.ci[s], t = c.ptype[p];
    std::vector<int> to, flag;
    if (t == 2 || t == 6) {
      const std::vector<int>& tg = t == 2 ? T.knight_ci[cs] : T.king_ci[cs];
      for (int tci : tg) {
        for (int d : x.by_ci[tci]) {
          if (x.sqcol[d] != side) {
            to.push_back(d);
            flag.push_back(0);
          }
        }
      }
    } else if (t >= 3 && t <= 5) {
      int k0 = t == 3 ? 5 : 1, k1 = t == 4 ? 4 : 8;
      for (int k = k0; k <= k1; ++k) {
        for (int tci : T.ray_ci[cs][k]) {
          for (int d : x.by_ci[tci]) {
            if (x.sqcol[d] != side) {
              to.push_back(d);
              flag.push_back(0);
            }
          }
          if (x.cnt[tci] > 0) break;
        }
      }
    } else {
      int dir = PAWN_DIR[side];
      int r = x.rank[s], tr = r + dir;
      if (tr >= 0 && tr <= 9) {
        int c1 = cs + dir;
        for (int d : x.by_ci[c1]) {
          if (c.occ[d] == 0) {
            to.push_back(d);
            flag.push_back(0);
          }
        }
        int tr2 = r + 2 * dir;
        if (!c.pmoved[p] && tr2 >= 0 && tr2 <= 9 && x.cnt[c1] == 0) {
          for (int d : x.by_ci[cs + 2 * dir]) {
            if (c.occ[d] == 0) {
              to.push_back(d);
              flag.push_back(1);
            }
          }
        }
        int f = (cs - 1) / 10;
        for (int df : {-10, 10}) {
          int tf = f + df / 10;
          if (tf < 0 || tf > 5) continue;
          int tci = c1 + df;
          for (int d : x.by_ci[tci]) {
            if (x.sqcol[d] == enemy) {
              to.push_back(d);
              flag.push_back(0);
            }
          }
          if (tci == ep_ci) {
            for (int d : x.by_ci[tci]) {
              if (c.occ[d] == 0) {
                to.push_back(d);
                flag.push_back(2);
              }
            }
          }
        }
      }
    }
    push_moves(out, T, c, x, p, s, to, flag);
  }
  for (int pl = 1; pl <= 4; ++pl) {
    int base = 48 + (pl - 1) * 4;
    int occp = 0, nocc = 0;
    for (int k = 1; k <= 4; ++k) {
      if (c.occ[base + k] > 0) {
        ++nocc;
        occp = c.occ[base + k];
      }
    }
    if (nocc != 1 || c.pcolor[occp] != side) continue;
    std::vector<int> fr = free_docks(T, c, pl);
    if (fr.empty()) continue;
    int psq = c.psq[occp];
    int cell = psq - (base + 1) + 1;
    for (int nd : fr) {
      for (int rot : {0, 180}) {
        bool pro = false;
        if (c.ptype[occp] == 1) {
          int no = (c.orient[pl] + rot) % 360;
          int nrk = T.plat_rank[nd][no == 180 ? 2 : 1][cell];
          int dk[5];
          for (int q = 1; q <= 4; ++q) dk[q] = c.dock[q];
          dk[pl] = nd;
          pro = promotes(T, side, psq, nrk, dk);
        }
        if (pro) {
          for (int pt : {5, 4, 3, 2}) out.push_back(Act{2, occp, psq, 0, 0, pt, pl, nd, rot, 0, 0});
        } else {
          out.push_back(Act{2, occp, psq, 0, 0, 0, pl, nd, rot, 0, 0});
        }
      }
    }
  }
  return out;
}

int count_pseudo(const Tables& T, const Core& c, const Ctx& x, int side) {
  int enemy = 3 - side, total = 0;
  for (int p = 1; p <= npieces(c); ++p) {
    if (c.pcolor[p] != side || c.psq[p] <= 0) continue;
    int s = c.psq[p], cs = x.ci[s], t = c.ptype[p];
    if (t == 2 || t == 6) {
      const std::vector<int>& tg = t == 2 ? T.knight_ci[cs] : T.king_ci[cs];
      for (int tci : tg) for (int d : x.by_ci[tci]) if (x.sqcol[d] != side) ++total;
    } else if (t >= 3 && t <= 5) {
      int k0 = t == 3 ? 5 : 1, k1 = t == 4 ? 4 : 8;
      for (int k = k0; k <= k1; ++k) {
        for (int tci : T.ray_ci[cs][k]) {
          for (int d : x.by_ci[tci]) if (x.sqcol[d] != side) ++total;
          if (x.cnt[tci] > 0) break;
        }
      }
    } else {
      int dir = PAWN_DIR[side];
      int r = x.rank[s], tr = r + dir;
      if (tr < 0 || tr > 9) continue;
      int c1 = cs + dir;
      std::vector<int> to;
      for (int d : x.by_ci[c1]) if (c.occ[d] == 0) to.push_back(d);
      int tr2 = r + 2 * dir;
      if (!c.pmoved[p] && tr2 >= 0 && tr2 <= 9 && x.cnt[c1] == 0) {
        for (int d : x.by_ci[cs + 2 * dir]) if (c.occ[d] == 0) to.push_back(d);
      }
      int f = (cs - 1) / 10;
      if (f > 0) for (int d : x.by_ci[c1 - 10]) if (x.sqcol[d] == enemy) to.push_back(d);
      if (f < 5) for (int d : x.by_ci[c1 + 10]) if (x.sqcol[d] == enemy) to.push_back(d);
      for (int d : to) total += promotes(T, side, d, x.rank[d], c.dock) ? 4 : 1;
    }
  }
  for (int pl = 1; pl <= 4; ++pl) {
    int base = 48 + (pl - 1) * 4;
    int occp = 0, nocc = 0;
    for (int k = 1; k <= 4; ++k) {
      if (c.occ[base + k] > 0) {
        ++nocc;
        occp = c.occ[base + k];
      }
    }
    if (nocc != 1 || c.pcolor[occp] != side) continue;
    std::vector<int> fr = free_docks(T, c, pl);
    if (fr.empty()) continue;
    if (c.ptype[occp] != 1) {
      total += 2 * static_cast<int>(fr.size());
      continue;
    }
    int cell = c.psq[occp] - (base + 1) + 1;
    for (int nd : fr) {
      for (int rot : {0, 180}) {
        int no = (c.orient[pl] + rot) % 360;
        int nrk = T.plat_rank[nd][no == 180 ? 2 : 1][cell];
        int dk[5];
        for (int q = 1; q <= 4; ++q) dk[q] = c.dock[q];
        dk[pl] = nd;
        total += promotes(T, side, c.psq[occp], nrk, dk) ? 4 : 1;
      }
    }
  }
  return total;
}

bool safe_after(const Tables& T, const Core& c, const Ctx& x, const Act& a) {
  int side = c.side;
  std::vector<int> psq = c.psq;
  int file[65], rank[65], cnt[61];
  for (int s = 1; s <= 64; ++s) {
    file[s] = x.file[s];
    rank[s] = x.rank[s];
  }
  for (int i = 1; i <= 60; ++i) cnt[i] = x.cnt[i];
  if (a.type == 1) {
    cnt[x.ci[a.from]]--;
    if (a.cap > 0) {
      int vs = psq[a.cap];
      cnt[x.ci[vs]]--;
      psq[a.cap] = 0;
    }
    cnt[x.ci[a.to]]++;
    psq[a.piece] = a.to;
  } else {
    int dock[5], orient[5];
    for (int q = 1; q <= 4; ++q) {
      dock[q] = c.dock[q];
      orient[q] = c.orient[q];
    }
    dock[a.plat] = a.dock;
    orient[a.plat] = (orient[a.plat] + a.rot) % 360;
    square_coords(T, dock, orient, file, rank);
    for (int i = 1; i <= 60; ++i) cnt[i] = 0;
    for (int s = 1; s <= 64; ++s) if (c.occ[s] > 0) cnt[file[s] * 10 + rank[s] + 1]++;
  }
  int ks = 0;
  for (int i = 1; i <= npieces(c); ++i) if (c.ptype[i] == 6 && c.pcolor[i] == side) ks = psq[i];
  return !attacked(file[ks], rank[ks], 3 - side, file, rank, cnt, c, psq);
}

std::vector<Act> castles(const Tables& T, const Core& c, const Ctx& x) {
  std::vector<Act> out;
  int side = c.side;
  if (!c.first[side]) return out;
  int enemy = 3 - side;
  for (int cs = 1; cs <= 2; ++cs) {
    if (!c.castle[(side - 1) * 2 + cs]) continue;
    const CastleDef& D = T.castle[side][cs];
    int k = 0, rk = 0;
    for (int i = 1; i <= npieces(c); ++i) {
      if (c.pid[i] == D.king && k == 0) k = i;
      if (c.pid[i] == D.rook && rk == 0) rk = i;
    }
    if (k == 0 || rk == 0) continue;
    if (c.psq[k] != D.king_from || c.psq[rk] != D.rook_from) continue;
    if (c.ptype[k] != 6 || c.ptype[rk] != 4) continue;
    bool home = true;
    for (size_t i = 0; i < D.home_platforms.size(); ++i) {
      int pl = D.home_platforms[i];
      if (c.dock[pl] != D.home_docks[i] || c.orient[pl] != 0) home = false;
    }
    if (!home) continue;
    bool blocked = false;
    for (int s : D.empty) if (c.occ[s] != 0) blocked = true;
    for (auto& cc : D.clear) if (x.cnt[cc.first * 10 + cc.second + 1] > 0) blocked = true;
    if (blocked) continue;
    if (attacked(x.file[D.king_from], x.rank[D.king_from], enemy, x.file, x.rank, x.cnt, c, c.psq)) continue;
    if (!D.probe.empty()) {
      int cnt[61];
      for (int i = 1; i <= 60; ++i) cnt[i] = x.cnt[i];
      cnt[x.ci[D.king_from]]--;
      std::vector<int> psq = c.psq;
      psq[k] = 0;
      bool hit = false;
      for (auto& pb : D.probe) {
        if (attacked(pb.first, pb.second, enemy, x.file, x.rank, cnt, c, psq)) {
          hit = true;
          break;
        }
      }
      if (hit) continue;
    }
    std::vector<int> psq = c.psq;
    psq[k] = D.king_to;
    psq[rk] = D.rook_to;
    int occ[65];
    for (int s = 1; s <= 64; ++s) occ[s] = c.occ[s];
    occ[D.king_from] = 0;
    occ[D.rook_from] = 0;
    occ[D.king_to] = k;
    occ[D.rook_to] = rk;
    int cnt[61];
    for (int i = 1; i <= 60; ++i) cnt[i] = 0;
    for (int s = 1; s <= 64; ++s) if (occ[s] > 0) cnt[x.ci[s]]++;
    if (attacked(x.file[D.king_to], x.rank[D.king_to], enemy, x.file, x.rank, cnt, c, psq)) continue;
    out.push_back(Act{3, k, D.king_from, D.king_to, 0, 0, 0, 0, 0, cs, 0});
  }
  return out;
}

std::string action_id(const Tables& T, const Core& c, const Act& a) {
  std::string promo = a.promo > 0 ? std::string("=") + TYPE_CHARS[a.promo] : "";
  if (a.type == 1) return "m:" + T.sq_id[a.from] + "-" + T.sq_id[a.to] + promo;
  if (a.type == 2) {
    return "t:" + std::string(PLATFORMS[a.plat]) + ">" + T.dock_id[a.dock] + "@" + std::to_string(a.rot) + promo;
  }
  return "c:" + std::string(COLORS[c.pcolor[a.piece]]) + ":" + (a.cside == 1 ? "kingside" : "queenside");
}

std::vector<Act> legal(const Tables& T, const Core& c, const Ctx& x) {
  std::vector<Act> ps = pseudo(T, c, x);
  std::vector<Act> out;
  if (!ps.empty()) {
    int side = c.side, kp = 0;
    for (int i = 1; i <= npieces(c); ++i) if (c.ptype[i] == 6 && c.pcolor[i] == side && c.psq[i] > 0) kp = i;
    int ks = c.psq[kp];
    int kf = x.file[ks], kr = x.rank[ks];
    bool chk = attacked(kf, kr, 3 - side, x.file, x.rank, x.cnt, c, c.psq);
    for (const Act& a : ps) {
      bool need = true;
      if (!chk) {
        int df = x.file[a.from] - kf, dr = x.rank[a.from] - kr;
        bool aligned = df == 0 || dr == 0 || std::abs(df) == std::abs(dr);
        need = aligned || a.piece == kp || a.flag == 2;
      }
      if (!need || safe_after(T, c, x, a)) out.push_back(a);
    }
  }
  std::vector<Act> cs = castles(T, c, x);
  out.insert(out.end(), cs.begin(), cs.end());
  if (out.size() > 1) {
    std::vector<std::pair<std::string, size_t>> keyed;
    for (size_t i = 0; i < out.size(); ++i) keyed.push_back({action_id(T, c, out[i]), i});
    std::sort(keyed.begin(), keyed.end());
    std::vector<Act> sorted;
    for (auto& kv : keyed) sorted.push_back(out[kv.second]);
    out.swap(sorted);
  }
  return out;
}

bool ep_effective(const Tables& T, const Core& c, const Ctx& x) {
  if (!c.has_ep) return false;
  int side = c.side, v = c.ep[1];
  if (c.psq[v] != c.ep[2] || c.pcolor[v] == side) return false;
  int tf = c.ep[3], tr = c.ep[4];
  int dir = PAWN_DIR[side];
  std::vector<int> dest;
  for (int s = 1; s <= 64; ++s) if (x.ci[s] == tf * 10 + tr + 1 && c.occ[s] == 0) dest.push_back(s);
  if (dest.empty()) return false;
  for (int p = 1; p <= npieces(c); ++p) {
    if (c.pcolor[p] != side || c.ptype[p] != 1 || c.psq[p] <= 0) continue;
    int s = c.psq[p];
    if (x.rank[s] + dir == tr && std::abs(x.file[s] - tf) == 1) {
      for (int d : dest) {
        Act a{1, p, s, d, v, 0, 0, 0, 0, 0, 2};
        if (safe_after(T, c, x, a)) return true;
      }
    }
  }
  return false;
}

std::string key_text(const Tables& T, const Core& c) {
  std::string t = T.rules_hash + "|" + std::to_string(c.side) + "|";
  for (int s = 1; s <= 64; ++s) {
    int code = 0;
    int pi = c.occ[s];
    if (pi > 0) code = (c.pcolor[pi] - 1) * 7 + c.ptype[pi] + ((c.ptype[pi] == 1 && !c.pmoved[pi]) ? 6 : 0);
    if (s > 1) t += ".";
    t += std::to_string(code);
  }
  t += "|";
  for (int p = 1; p <= 4; ++p) {
    if (p > 1) t += ".";
    t += std::to_string(c.dock[p]);
  }
  t += "|";
  for (int p = 1; p <= 4; ++p) {
    if (p > 1) t += ".";
    t += std::to_string(c.orient[p]);
  }
  t += "|";
  for (int i = 1; i <= 4; ++i) t += c.castle[i] ? "1" : "0";
  t += "|";
  t += (c.first[1] && (c.castle[1] || c.castle[2])) ? "1" : "0";
  t += (c.first[2] && (c.castle[3] || c.castle[4])) ? "1" : "0";
  t += "|";
  std::string ep = "-";
  if (c.has_ep) {
    Ctx x;
    make_ctx(T, c, x);
    if (ep_effective(T, c, x)) {
      ep = std::to_string(c.ep[3]) + "." + std::to_string(c.ep[4]) + "." + std::to_string(c.ep[2]);
    }
  }
  t += ep;
  return t;
}

void revoke_mover(const Tables& T, Core& c, int p, int side) {
  const std::string& id = c.pid[p];
  if (id == T.castle[side][1].king) {
    c.castle[(side - 1) * 2 + 1] = false;
    c.castle[(side - 1) * 2 + 2] = false;
  }
  for (int cs = 1; cs <= 2; ++cs) if (id == T.castle[side][cs].rook) c.castle[(side - 1) * 2 + cs] = false;
}

void revoke_rook(const Tables& T, Core& c, int v) {
  int col = c.pcolor[v];
  for (int cs = 1; cs <= 2; ++cs) if (c.pid[v] == T.castle[col][cs].rook) c.castle[(col - 1) * 2 + cs] = false;
}

Core make(const Tables& T, const Core& c0, const Act& a) {
  Core c = c0;
  int side = c.side;
  int p = a.piece;
  c.has_ep = false;
  for (int i = 1; i <= 4; ++i) c.ep[i] = 0;
  bool irreversible = false, reset_half = false;
  if (a.type == 1) {
    if (a.cap > 0) {
      int v = a.cap;
      c.occ[c.psq[v]] = 0;
      revoke_rook(T, c, v);
      c.psq[v] = 0;
      irreversible = true;
      reset_half = true;
    }
    c.occ[a.from] = 0;
    c.occ[a.to] = p;
    c.psq[p] = a.to;
    c.pmoved[p] = 1;
    if (c.ptype[p] == 1) {
      reset_half = true;
      if (a.flag == 1) {
        int file[65], rank[65];
        square_coords(T, c.dock, c.orient, file, rank);
        c.has_ep = true;
        c.ep[1] = p;
        c.ep[2] = a.to;
        c.ep[3] = file[a.from];
        c.ep[4] = rank[a.from] + PAWN_DIR[side];
      }
    }
    if (a.promo > 0) {
      c.ptype[p] = a.promo;
      irreversible = true;
    }
    revoke_mover(T, c, p, side);
  } else if (a.type == 2) {
    c.dock[a.plat] = a.dock;
    c.orient[a.plat] = (c.orient[a.plat] + a.rot) % 360;
    c.pmoved[p] = 1;
    if (c.ptype[p] == 1) reset_half = true;
    if (a.promo > 0) {
      c.ptype[p] = a.promo;
      irreversible = true;
    }
    revoke_mover(T, c, p, side);
  } else {
    const CastleDef& D = T.castle[side][a.cside];
    int rk = 0;
    for (int i = 1; i <= npieces(c); ++i) if (c.pid[i] == D.rook) { rk = i; break; }
    c.occ[D.king_from] = 0;
    c.occ[D.rook_from] = 0;
    c.occ[D.king_to] = p;
    c.occ[D.rook_to] = rk;
    c.psq[p] = D.king_to;
    c.psq[rk] = D.rook_to;
    c.pmoved[p] = 1;
    c.pmoved[rk] = 1;
    c.castle[(side - 1) * 2 + 1] = false;
    c.castle[(side - 1) * 2 + 2] = false;
  }
  c.halfmove = reset_half ? 0 : c.halfmove + 1;
  c.first[side] = true;
  c.side = 3 - side;
  c.ply = c.ply + 1;
  std::string key = sha256_hex(key_text(T, c));
  if (irreversible) c.rep.clear();
  c.rep.push_back(key);
  return c;
}

double evaluate_position(const Tables& T, const Core& c, const Ctx& x) {
  int material[3] = {0, 0, 0}, kn[3] = {0, 0, 0}, chk[3] = {0, 0, 0}, pawn[3] = {0, 0, 0}, plat[3] = {0, 0, 0};
  int n = npieces(c);
  for (int i = 1; i <= n; ++i) if (c.psq[i] > 0) material[c.pcolor[i]] += PIECE_VALUE[c.ptype[i]];
  int mob1 = count_pseudo(T, c, x, 1), mob2 = count_pseudo(T, c, x, 2);
  for (int s = 1; s <= 2; ++s) {
    int ks = king_sq(c, s);
    if (ks == 0) continue;
    for (int i = 1; i <= n; ++i) {
      if (c.psq[i] <= 0 || c.pcolor[i] != 3 - s || c.ptype[i] == 1 || c.ptype[i] == 6) continue;
      int es = c.psq[i];
      int d = std::max(std::abs(x.file[es] - x.file[ks]), std::abs(x.rank[es] - x.rank[ks]));
      if (d <= 2) kn[s]++;
    }
    chk[s] = attacked(x.file[ks], x.rank[ks], 3 - s, x.file, x.rank, x.cnt, c, c.psq) ? 1 : 0;
  }
  for (int i = 1; i <= n; ++i) {
    if (c.psq[i] <= 0 || c.ptype[i] != 1) continue;
    int r = x.rank[c.psq[i]];
    if (c.pcolor[i] == 1) pawn[1] += std::max(r - 1, 0);
    else pawn[2] += std::max(8 - r, 0);
  }
  for (int pl = 1; pl <= 4; ++pl) {
    int base = 48 + (pl - 1) * 4, occp = 0, nocc = 0;
    for (int k = 1; k <= 4; ++k) {
      if (c.occ[base + k] > 0) {
        ++nocc;
        occp = c.occ[base + k];
      }
    }
    if (nocc != 1) continue;
    plat[c.pcolor[occp]] += static_cast<int>(free_docks(T, c, pl).size());
  }
  int white = material[1] - material[2] + 4 * (mob1 - mob2) - 25 * (kn[1] - kn[2]) - 40 * (chk[1] - chk[2]) +
              6 * (pawn[1] - pawn[2]) + 8 * (plat[1] - plat[2]);
  return c.side == 1 ? white : -white;
}

bool rule_draw(const Core& c) {
  const std::string& key = c.rep.back();
  int n = 0;
  for (const auto& k : c.rep) if (k == key) ++n;
  if (n >= 3 || c.halfmove >= 100) return true;
  int alive = 0;
  bool kings = true;
  for (int i = 1; i <= npieces(c); ++i) {
    if (c.psq[i] > 0) {
      ++alive;
      if (c.ptype[i] != 6) kings = false;
    }
  }
  return alive == 2 && kings;
}

// R: order(-key, seq_len(n)); stable descending by key.
std::vector<Act> order_moves(const Core& c, const std::vector<Act>& m, int tt_move) {
  std::vector<Act> out;
  out.reserve(m.size());
  // Element-wise copies: copy-constructing the vector here triggers a false
  // -Wstringop-overflow warning in g++ 12/13.
  if (m.size() <= 1) {
    for (const Act& a : m) out.push_back(a);
    return out;
  }
  std::vector<std::pair<double, size_t>> k;
  for (size_t i = 0; i < m.size(); ++i) {
    const Act& a = m[i];
    double victim = a.cap > 0 ? PIECE_VALUE[c.ptype[a.cap]] : 0;
    double attacker = PIECE_VALUE[c.ptype[a.piece]];
    double key = victim * 10.0 - attacker / 100.0 + (a.promo > 0 ? 5000.0 : 0.0) + (a.type == 3 ? 50.0 : 0.0) +
                 (a.type == 2 ? 20.0 : 0.0);
    if (static_cast<int>(i) == tt_move) key = 1e7;
    k.push_back({key, i});
  }
  std::stable_sort(k.begin(), k.end(), [](const std::pair<double, size_t>& a, const std::pair<double, size_t>& b) {
    return a.first > b.first;
  });
  for (auto& kv : k) out.push_back(m[kv.second]);
  return out;
}

// ---------------------------------------------------------------- search

struct TTEntry {
  int depth;
  double score;
  int flag;
  std::string move;
};

struct Search {
  const Tables* T;
  double nodes = 0;
  double max_nodes;
  double time_ms;
  std::chrono::steady_clock::time_point t0;
  bool stop = false;
  std::unordered_map<std::string, TTEntry> tt;
  int tt_n = 0;
  Function* stop_check = nullptr;
};

std::string tt_key(const Core& c) {
  std::string cat;
  cat.reserve(c.rep.size() * 64);
  for (const auto& k : c.rep) cat += k;
  return c.rep.back() + "|" + std::to_string(c.rep.size()) + "|" + cat + "|" + std::to_string(c.halfmove);
}

bool tick(Search& e) {
  e.nodes += 1;
  if (e.nodes > e.max_nodes) e.stop = true;
  if (std::isfinite(e.time_ms) && std::fmod(e.nodes, 16.0) == 0.0) {
    double el = std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - e.t0).count();
    if (el > e.time_ms) e.stop = true;
  }
  if (e.stop_check != nullptr && std::fmod(e.nodes, 64.0) == 0.0) {
    LogicalVector r = (*e.stop_check)();
    if (r.size() == 1 && r[0] == TRUE) e.stop = true;
  }
  return e.stop;
}

double tt_adjust(double s, int ply) {
  if (s > MATE_BOUND) return s + ply;
  if (s < -MATE_BOUND) return s - ply;
  return s;
}

double tt_unadjust(double s, int ply) {
  if (s > MATE_BOUND) return s - ply;
  if (s < -MATE_BOUND) return s + ply;
  return s;
}

double qsearch(Search& e, const Core& c, double alpha, double beta, int ply, int qd, const Ctx* xin);

double negamax(Search& e, const Core& c, int depth, double alpha, double beta, int ply) {
  if (tick(e)) return 0;
  const Tables& T = *e.T;
  if (ply > 0 && rule_draw(c)) return 0;
  Ctx x;
  make_ctx(T, c, x);
  std::vector<Act> lg = legal(T, c, x);
  if (lg.empty()) return in_check(c, c.side, x) ? -(MATE - ply) : 0;
  if (depth <= 0) return qsearch(e, c, alpha, beta, ply, 0, &x);
  std::string ttk = tt_key(c);
  int tt_move = -1;
  auto it = e.tt.find(ttk);
  if (it != e.tt.end()) {
    const TTEntry& ent = it->second;
    for (size_t i = 0; i < lg.size(); ++i) {
      if (action_id(T, c, lg[i]) == ent.move) {
        tt_move = static_cast<int>(i);
        break;
      }
    }
    if (ent.depth >= depth) {
      double s = tt_unadjust(ent.score, ply);
      if (ent.flag == 0) return s;
      if (ent.flag == 1 && s >= beta) return s;
      if (ent.flag == 2 && s <= alpha) return s;
    }
  }
  lg = order_moves(c, lg, tt_move);
  double alpha0 = alpha, best = -INFINITY;
  size_t best_i = 0;
  for (size_t i = 0; i < lg.size(); ++i) {
    Core child = make(T, c, lg[i]);
    double s = -negamax(e, child, depth - 1, -beta, -alpha, ply + 1);
    if (e.stop) return 0;
    if (s > best) {
      best = s;
      best_i = i;
    }
    if (s > alpha) alpha = s;
    if (alpha >= beta) break;
  }
  int flag = best <= alpha0 ? 2 : (best >= beta ? 1 : 0);
  if (e.tt_n > 200000) {
    e.tt.clear();
    e.tt_n = 0;
  }
  e.tt[ttk] = TTEntry{depth, tt_adjust(best, ply), flag, action_id(T, c, lg[best_i])};
  e.tt_n++;
  return best;
}

double qsearch(Search& e, const Core& c, double alpha, double beta, int ply, int qd, const Ctx* xin) {
  if (tick(e)) return 0;
  const Tables& T = *e.T;
  if (qd > 0 && rule_draw(c)) return 0;
  Ctx xl;
  if (xin == nullptr) make_ctx(T, c, xl);
  const Ctx& x = xin == nullptr ? xl : *xin;
  bool chk = in_check(c, c.side, x);
  std::vector<Act> moves;
  double best;
  if (chk) {
    moves = legal(T, c, x);
    if (moves.empty()) return -(MATE - ply);
    if (qd >= QDEPTH) return evaluate_position(T, c, x);
    best = -INFINITY;
  } else {
    double stand = evaluate_position(T, c, x);
    if (stand >= beta || qd >= QDEPTH) return stand;
    if (stand > alpha) alpha = stand;
    best = stand;
    std::vector<Act> ps = pseudo(T, c, x);
    std::vector<Act> tac;
    for (const Act& a : ps) if (a.cap > 0 || a.promo > 0) tac.push_back(a);
    if (tac.empty()) return stand;
    for (const Act& a : tac) if (safe_after(T, c, x, a)) moves.push_back(a);
    if (moves.empty()) return stand;
  }
  moves = order_moves(c, moves, -1);
  for (size_t i = 0; i < moves.size(); ++i) {
    Core child = make(T, c, moves[i]);
    double s = -qsearch(e, child, -beta, -alpha, ply + 1, qd + 1, nullptr);
    if (e.stop) return 0;
    if (s > best) best = s;
    if (s > alpha) alpha = s;
    if (alpha >= beta) break;
  }
  return best;
}

}  // namespace

// ---------------------------------------------------------------- R entry points

// [[Rcpp::export(.g3_cpp_tables)]]
SEXP g3_cpp_tables(List tabs) {
  XPtr<Tables> ptr(build_tables(tabs), true);
  return ptr;
}

// [[Rcpp::export(.g3_cpp_sha256)]]
std::string g3_cpp_sha256(std::string x) { return sha256_hex(x); }

// Legal actions as an integer matrix in the R column layout, sorted by id.
// [[Rcpp::export(.g3_cpp_legal)]]
IntegerMatrix g3_cpp_legal(SEXP tables, List core) {
  XPtr<Tables> T(tables);
  Core c = core_from_list(core);
  Ctx x;
  make_ctx(*T, c, x);
  std::vector<Act> lg = legal(*T, c, x);
  IntegerMatrix m(lg.size(), 11);
  for (size_t i = 0; i < lg.size(); ++i) {
    const Act& a = lg[i];
    int v[11] = {a.type, a.piece, a.from, a.to, a.cap, a.promo, a.plat, a.dock, a.rot, a.cside, a.flag};
    for (int j = 0; j < 11; ++j) m(i, j) = v[j];
  }
  return m;
}

// [[Rcpp::export(.g3_cpp_key_text)]]
std::string g3_cpp_key_text(SEXP tables, List core) {
  XPtr<Tables> T(tables);
  return key_text(*T, core_from_list(core));
}

// [[Rcpp::export(.g3_cpp_eval)]]
double g3_cpp_eval(SEXP tables, List core) {
  XPtr<Tables> T(tables);
  Core c = core_from_list(core);
  Ctx x;
  make_ctx(*T, c, x);
  return evaluate_position(*T, c, x);
}

// Root search with R's semantics; see .g3_root_search() in R/search.R.
// [[Rcpp::export(.g3_cpp_root_search)]]
List g3_cpp_root_search(SEXP tables, List core, double max_nodes, double time_ms, int max_depth,
                        bool exact_alternatives, Nullable<CharacterVector> restrict_ids,
                        Nullable<Function> stop_check) {
  XPtr<Tables> Tp(tables);
  const Tables& T = *Tp;
  Search e;
  e.T = Tp.get();
  e.max_nodes = max_nodes;
  e.time_ms = time_ms;
  e.t0 = std::chrono::steady_clock::now();
  Function* sc = nullptr;
  std::unique_ptr<Function> sc_holder;
  if (stop_check.isNotNull()) {
    sc_holder.reset(new Function(stop_check.get()));
    sc = sc_holder.get();
  }
  e.stop_check = sc;
  Core c = core_from_list(core);
  Ctx x;
  make_ctx(T, c, x);
  std::vector<Act> lg = legal(T, c, x);
  std::vector<std::string> ids;
  for (const Act& a : lg) ids.push_back(action_id(T, c, a));
  if (restrict_ids.isNotNull()) {
    CharacterVector r(restrict_ids.get());
    std::vector<std::string> rs;
    for (int i = 0; i < r.size(); ++i) rs.push_back(as<std::string>(r[i]));
    std::vector<Act> lg2;
    std::vector<std::string> ids2;
    for (size_t i = 0; i < lg.size(); ++i) {
      if (std::find(rs.begin(), rs.end(), ids[i]) != rs.end()) {
        lg2.push_back(lg[i]);
        ids2.push_back(ids[i]);
      }
    }
    lg.swap(lg2);
    ids.swap(ids2);
  }
  int n = static_cast<int>(lg.size());
  NumericVector scores_out(n, NA_REAL);
  CharacterVector bounds_out(n, "unknown");
  int depth_done = 0;
  int best_out = NA_INTEGER;
  std::string status = "terminal";
  CharacterVector pv_out(0);
  if (n == 0) {
    return List::create(_["depth"] = 0, _["best"] = NA_INTEGER, _["scores"] = scores_out,
                        _["bounds"] = bounds_out, _["status"] = status, _["nodes"] = 0.0,
                        _["elapsed"] = 0.0, _["pv"] = pv_out);
  }
  std::vector<Act> order0 = order_moves(c, lg, -1);
  std::vector<std::string> ord_ids;
  for (const Act& a : order0) ord_ids.push_back(action_id(T, c, a));
  std::vector<Core> children;
  for (const Act& a : order0) children.push_back(make(T, c, a));
  int best_completed = -1;
  for (int d = 1; d <= max_depth; ++d) {
    double alpha = -INFINITY, beta = INFINITY;
    std::vector<double> sc(n, NAN);
    std::vector<std::string> bd(n, "unknown");
    int best_i = -1;
    for (int i = 0; i < n; ++i) {
      double s;
      std::string b;
      if (exact_alternatives) {
        s = -negamax(e, children[i], d - 1, -INFINITY, INFINITY, 1);
        b = "exact";
      } else {
        s = -negamax(e, children[i], d - 1, -beta, -alpha, 1);
        b = (best_i < 0 || s > alpha) ? "exact" : "upper";
      }
      if (e.stop) break;
      sc[i] = s;
      bd[i] = b;
      if (best_i < 0 || s > sc[best_i]) best_i = i;
      if (s > alpha) alpha = s;
    }
    if (e.stop) break;
    depth_done = d;
    best_completed = best_i;
    for (int j = 0; j < n; ++j) {
      int k = static_cast<int>(std::find(ord_ids.begin(), ord_ids.end(), ids[j]) - ord_ids.begin());
      scores_out[j] = sc[k];
      bounds_out[j] = bd[k];
    }
    best_out = static_cast<int>(std::find(ids.begin(), ids.end(), ord_ids[best_i]) - ids.begin()) + 1;
    // Reorder: best first, then by descending score, ties by previous order.
    std::vector<int> idx;
    for (int i = 0; i < n; ++i) idx.push_back(i);
    std::stable_sort(idx.begin(), idx.end(), [&](int a, int b) { return sc[a] > sc[b]; });
    std::vector<int> o;
    o.push_back(best_i);
    for (int i : idx) if (i != best_i) o.push_back(i);
    std::vector<Act> o0;
    std::vector<std::string> oi;
    std::vector<Core> oc;
    for (int i : o) {
      o0.push_back(order0[i]);
      oi.push_back(ord_ids[i]);
      oc.push_back(children[i]);
    }
    order0.swap(o0);
    ord_ids.swap(oi);
    children.swap(oc);
    if (std::fabs(sc[best_i]) > MATE_BOUND) break;
  }
  double elapsed = std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - e.t0).count();
  if (best_completed < 0) {
    status = "fallback";
    std::vector<Act> o = order_moves(c, lg, -1);
    std::string first = action_id(T, c, o[0]);
    best_out = static_cast<int>(std::find(ids.begin(), ids.end(), first) - ids.begin()) + 1;
  } else {
    status = e.stop ? "budget" : "completed";
    // Principal variation: best root action, then TT best moves.
    std::vector<std::string> pv;
    std::string id = ids[best_out - 1];
    pv.push_back(id);
    Core cur = c;
    int kmax = std::max(depth_done, 1) + 2;
    for (int k = 1; k <= kmax; ++k) {
      Ctx cx;
      make_ctx(T, cur, cx);
      std::vector<Act> l2 = legal(T, cur, cx);
      int found = -1;
      for (size_t i = 0; i < l2.size(); ++i) {
        if (action_id(T, cur, l2[i]) == id) {
          found = static_cast<int>(i);
          break;
        }
      }
      if (found < 0) break;
      cur = make(T, cur, l2[found]);
      auto it = e.tt.find(tt_key(cur));
      if (it == e.tt.end() || k >= depth_done) break;
      id = it->second.move;
      pv.push_back(id);
    }
    pv_out = wrap(pv);
  }
  return List::create(_["depth"] = depth_done, _["best"] = best_out, _["scores"] = scores_out,
                      _["bounds"] = bounds_out, _["status"] = status, _["nodes"] = e.nodes,
                      _["elapsed"] = elapsed, _["pv"] = pv_out);
}

// [[Rcpp::export(.g3_cpp_ptr_valid)]]
bool g3_cpp_ptr_valid(SEXP p) { return TYPEOF(p) == EXTPTRSXP && R_ExternalPtrAddr(p) != nullptr; }
